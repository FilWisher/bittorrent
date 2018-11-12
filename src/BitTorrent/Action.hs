module BitTorrent.Action
    ( Action(..)
    ) where

import qualified Network.Socket as NS
import Data.Word

import BitTorrent.Protocol
import BitTorrent.Connection

data Action
    -- | Fires when a new incoming TCP socket is opened.
    = IncomingSocket NS.Socket
    -- | Fires when a new outgoing TCP socket is opened.
    | OutgoingSocket InfoHash NS.Socket
    -- | Fires when a handshake is complete and a connection is established.
    | HandshakeComplete InfoHash NodeID NS.Socket
    -- | Fires when a connection is closed or times out.
    | ConnectionClosed NodeID
    -- | Fires when the connection to peer id receives a message.
    | IncomingMessage NodeID Message
    -- | Fires at regular intervals when the timer expires. 
    | Tick
    -- | Fires when a connection to a new peer is needed.
    | Connect Word16 NS.HostAddress
    -- | Fires when a request for a new piece is needed.
    | NewRequest Connection Word32 Word32 Word32
    -- | Fires when an open request can be cancelled.
    | CancelRequest Connection Word32 Word32 Word32
    -- | Fires when a file downloaded is completed.
    | Complete
    deriving (Show)
