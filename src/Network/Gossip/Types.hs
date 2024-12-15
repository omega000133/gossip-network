{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}

module Network.Gossip.Types where

import Data.ByteString (ByteString)
import GHC.Generics (Generic)
import Data.Time.Clock (UTCTime)

-- | Represents a message in the gossip network
data GossipMessage = GossipMessage
    { messageId :: MessageId
    , payload   :: ByteString
    , timestamp :: UTCTime
    } deriving stock (Show, Eq, Generic)

newtype MessageId = MessageId ByteString
    deriving stock (Show, Eq, Ord, Generic)

-- | Node identifier
newtype NodeId = NodeId ByteString
    deriving stock (Show, Eq, Ord, Generic)

-- | Network configuration
data GossipConfig = GossipConfig
    { fanout        :: Int    -- ^ Number of peers to forward messages to
    , heartbeatInt  :: Int    -- ^ Heartbeat interval in seconds
    , retryTimeout  :: Int    -- ^ Message retry timeout in seconds
    } deriving stock (Show, Eq, Generic)
