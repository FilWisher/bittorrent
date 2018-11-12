{-# LANGUAGE RecordWildCards #-}

module BitTorrent.Connection
    ( Connection(..)
    , closeConnection
    , createConnection
    , transitionConnection
    ) where

import qualified Network.Socket as NS
import           Control.Concurrent.Async

import Data.Bits (zeroBits)
import qualified Data.BitField as BitField

import BitTorrent.Protocol

-- | Represents the state of one side of the connection.
data Node = Node
    -- | Is this side of the connection being choked?
    { choked     :: Bool
    -- | Does this side of the connection have pieces we're interested in?
    , interested :: Bool
    }

-- | Represents an open connection to a peer.
data Connection = Connection
    -- | The pieces the peer (probably) has.
    { connPieces :: BitField.BitField
    -- | The choked and interested state of us to the peer.
    , connSelf   :: Node
    -- | The choked and interested state of the peer to us.
    , connPeer   :: Node
    -- | The socket used to communicate with the peer.
    , connSocket :: NS.Socket
    -- | The thread listening for incoming messages from the peer.
    , connThread :: Async ()
    -- | The ID of the peer.
    , connId     :: NodeID
    }

instance Show Connection where
    show _ = "Connection{..}"

-- | Spawns a listening action in a thread and return a Connection handle.
createConnection :: NS.Socket -> NodeID -> IO () -> IO Connection
createConnection sock peerid fn = do
    thread <- async fn
    return $ Connection
        { connPieces = zeroBits
        , connSelf   = Node False False
        , connPeer   = Node False False
        , connSocket = sock
        , connThread = thread
        , connId     = peerid
        }

-- | Closes an open connection: cancels the listening thread and closes the
--   socket.
closeConnection :: Connection -> IO ()
closeConnection conn = do
    NS.close (connSocket conn)
    cancel (connThread conn)

-- | Transition the connection state with a Message.
transitionConnection :: Message -> Connection -> Connection
transitionConnection msg conn@Connection{..} =
    case msg of
        -- XXX: Track last msg received by conn and update on KeepAlive.
        KeepAlive         -> conn
        Choke             -> conn { connSelf   = connSelf { choked     = True  } }
        Unchoke           -> conn { connSelf   = connSelf { choked     = False } }
        Interested        -> conn { connSelf   = connSelf { interested = True  } }
        NotInterested     -> conn { connSelf   = connSelf { interested = False } }
        BitField bitfield -> conn { connPieces = bitfield }
        _                 -> conn
