module Network.Transport where

import Network.Gossip.Types
import Control.Concurrent.STM
import Control.Concurrent (forkIO)
import Control.Monad (forever, void, unless)
import qualified Network.Socket as Socket
import qualified Network.Socket.ByteString as SocketBS
import qualified Data.ByteString as BS
import Data.Binary (Binary, encode, decode)
import qualified Data.Binary as Binary
import Control.Exception (bracket, try, SomeException)

-- | Enhanced Transport with connection management
data Transport = Transport
    { sendMessage    :: NodeId -> GossipMessage -> IO ()
    , receiveMessage :: IO GossipMessage
    , closeTransport :: IO ()
    }

-- | Connection pool
data ConnectionPool = ConnectionPool
    { connections :: TVar [(NodeId, Socket.Socket)]
    }

-- | Initialize connection pool
newConnectionPool :: IO ConnectionPool
newConnectionPool = ConnectionPool <$> newTVarIO []

-- | Create a TCP transport with proper connection handling
createTcpTransport :: Socket.PortNumber -> IO Transport
createTcpTransport port = do
    sock <- Socket.socket Socket.AF_INET Socket.Stream Socket.defaultProtocol
    Socket.setSocketOption sock Socket.ReuseAddr 1
    Socket.bind sock (Socket.SockAddrInet port Socket.iNADDR_ANY)
    Socket.listen sock 5
    
    pool <- newConnectionPool
    messageQueue <- newTQueueIO
    
    -- Accept connections in background
    void $ forkIO $ forever $ do
        (clientSock, _) <- Socket.accept sock
        void $ forkIO $ handleClient clientSock messageQueue
    
    let send nodeId msg = do
            conn <- getConnection pool nodeId
            SocketBS.sendAll conn (Binary.encode msg)
            
        receive = atomically $ readTQueue messageQueue
        
        close = do
            atomically $ do
                conns <- readTVar (connections pool)
                mapM_ (Socket.close . snd) conns
            Socket.close sock
    
    pure $ Transport send receive close

-- | Handle incoming client connections
handleClient :: Socket.Socket -> TQueue GossipMessage -> IO ()
handleClient sock queue = forever $ do
    msg <- SocketBS.recv sock 4096
    unless (BS.null msg) $ do
        case Binary.decodeOrFail msg of
            Right (_, _, gossipMsg) ->
                atomically $ writeTQueue queue gossipMsg
            Left _ -> 
                pure () -- Handle error

-- | Get or create connection to a node
getConnection :: ConnectionPool -> NodeId -> IO Socket.Socket
getConnection pool nodeId = do
    mConn <- atomically $ do
        conns <- readTVar (connections pool)
        pure $ lookup nodeId conns
    case mConn of
        Just conn -> pure conn
        Nothing -> establishConnection pool nodeId

-- | Establish new connection to a node
establishConnection :: ConnectionPool -> NodeId -> IO Socket.Socket
establishConnection pool nodeId = do
    sock <- Socket.socket Socket.AF_INET Socket.Stream Socket.defaultProtocol
    -- Here resolve NodeId to actual network address
    -- For simplicity, assuming NodeId contains "host:port"
    -- In real implementation, proper node address resolution
    Socket.connect sock (Socket.SockAddrInet 8080 Socket.iNADDR_ANY)
    atomically $ modifyTVar' (connections pool) ((nodeId, sock):)
    pure sock