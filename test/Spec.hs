{-# LANGUAGE OverloadedStrings #-}

module Main where

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck
import Network.Gossip.Types
import Network.Gossip.Protocol
import Network.Transport
import Data.Time.Clock
import qualified Data.ByteString as BS
import Control.Concurrent (threadDelay)
import Control.Monad (replicateM)
import qualified Data.Set as Set

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Gossip Protocol Tests"
    [ testCase "Message Broadcasting" testMessageBroadcast
    , testCase "Peer Management" testPeerManagement
    , testCase "Message Propagation" testMessagePropagation
    , testProperty "Message IDs are unique" propMessageIdUnique
    ]

testMessageBroadcast :: Assertion
testMessageBroadcast = do
    let config = GossipConfig 3 1 5
    state <- initGossipState config
    now <- getCurrentTime
    let msg = GossipMessage
            { messageId = MessageId "test-message"
            , payload = "test payload"
            , timestamp = now
            }
    
    gossipBroadcast state msg
    received <- gossipReceive state
    
    case received of
        Just receivedMsg -> messageId receivedMsg @?= messageId msg
        Nothing         -> assertFailure "No message received"

testPeerManagement :: Assertion
testPeerManagement = do
    let config = GossipConfig 3 1 5
    state <- initGossipState config
    let peer = NodeId "test-peer"
    
    addPeer state peer
    peers' <- atomically $ readTVar (peers state)
    assertBool "Peer should be added" $ peer `Set.member` peers'
    
    removePeer state peer
    peers'' <- atomically $ readTVar (peers state)
    assertBool "Peer should be removed" $ peer `Set.notMember` peers''

testMessagePropagation :: Assertion
testMessagePropagation = do
    transport <- createTcpTransport 8080
    let config = GossipConfig 3 1 5
    state <- initGossipState config
    
    startGossip state transport
    threadDelay 1000000  -- Wait for propagation
    
    closeTransport transport

propMessageIdUnique :: [BS.ByteString] -> Property
propMessageIdUnique msgs = 
    let messageIds = map MessageId msgs
    in length messageIds === length (Set.fromList messageIds)
