module Main where

import Network.Gossip.Protocol
import Network.Gossip.Types
import Network.Transport
import Control.Concurrent (threadDelay)
import Data.Time.Clock
import Control.Monad (forever)
import System.Environment (getArgs)

main :: IO ()
main = do
    args <- getArgs
    let port = case args of
            (p:_) -> read p
            _     -> 8080
    
    let config = GossipConfig
            { fanout = 3
            , heartbeatInt = 1
            , retryTimeout = 5
            }
    
    state <- initGossipState config
    transport <- createTcpTransport port
    
    startGossip state transport
    
    -- Example usage loop
    forever $ do
        now <- getCurrentTime
        let testMessage = GossipMessage
                { messageId = MessageId "test"
                , payload = "Hello, Gossip!"
                , timestamp = now
                }
        
        gossipBroadcast state testMessage
        threadDelay 1000000  -- Wait 1 second
        
        received <- gossipReceive state
        print received
