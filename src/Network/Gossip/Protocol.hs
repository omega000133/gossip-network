{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}

module Network.Gossip.Protocol where

import Network.Gossip.Types
import Control.Monad.IO.Class
import qualified Data.Set as Set
import Control.Concurrent.STM
import Control.Concurrent (forkIO, threadDelay)
import Control.Monad (forever, void, unless)
import Data.Time.Clock
import System.Random (randomRIO)
import qualified Data.Map.Strict as Map

-- | Protocol state
data GossipState = GossipState
    { peers        :: TVar (Set.Set NodeId)
    , seenMessages :: TVar (Map.Map MessageId UTCTime)
    , messageQueue :: TQueue GossipMessage
    , config      :: GossipConfig
    }

-- | Initialize gossip state
initGossipState :: MonadIO m => GossipConfig -> m GossipState
initGossipState cfg = liftIO $ GossipState
    <$> newTVarIO Set.empty
    <*> newTVarIO Map.empty
    <*> newTQueueIO
    <*> pure cfg

-- | Start gossip protocol
startGossip :: GossipState -> Transport -> IO ()
startGossip state transport = do
    -- Start message propagation
    void $ forkIO $ forever $ do
        threadDelay (heartbeatInt (config state) * 1000000)
        propagateMessages state transport
    
    -- Start message cleanup
    void $ forkIO $ forever $ do
        threadDelay (retryTimeout (config state) * 1000000)
        cleanupOldMessages state

-- | Propagate messages to peers
propagateMessages :: GossipState -> Transport -> IO ()
propagateMessages state transport = do
    peerList <- atomically $ readTVar (peers state)
    messages <- atomically $ readTVar (seenMessages state)
    
    forM_ (Set.toList peerList) $ \peer -> do
        selectedMsgs <- selectRandomMessages messages (fanout $ config state)
        forM_ selectedMsgs $ \(msgId, _) -> do
            msg <- retrieveMessage state msgId
            case msg of
                Just m -> sendMessage transport peer m
                Nothing -> pure ()

-- | Select random messages for propagation
selectRandomMessages :: Map.Map MessageId UTCTime -> Int -> IO [(MessageId, UTCTime)]
selectRandomMessages messages count = do
    let msgList = Map.toList messages
    if length msgList <= count
        then pure msgList
        else do
            indices <- replicateM count (randomRIO (0, length msgList - 1))
            pure $ map (msgList !!) indices

-- | Clean up old messages
cleanupOldMessages :: GossipState -> IO ()
cleanupOldMessages state = do
    now <- getCurrentTime
    atomically $ modifyTVar' (seenMessages state) $ \msgs ->
        Map.filter (\timestamp -> 
            diffUTCTime now timestamp < fromIntegral (retryTimeout $ config state)) msgs

-- | Add a new peer
addPeer :: MonadIO m => GossipState -> NodeId -> m ()
addPeer state nodeId = liftIO $ atomically $
    modifyTVar' (peers state) (Set.insert nodeId)

-- | Remove a peer
removePeer :: MonadIO m => GossipState -> NodeId -> m ()
removePeer state nodeId = liftIO $ atomically $
    modifyTVar' (peers state) (Set.delete nodeId)

-- | Retrieve a message by ID
retrieveMessage :: GossipState -> MessageId -> IO (Maybe GossipMessage)
retrieveMessage state msgId = atomically $ do
    msgs <- readTVar (seenMessages state)
    if Map.member msgId msgs
        then tryReadTQueue (messageQueue state)
        else pure Nothing
