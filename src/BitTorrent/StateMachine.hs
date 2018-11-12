{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections   #-}

module BitTorrent.StateMachine
    ( Event(..)
    , StateMachine(..)
    
    , initialState
    , transition
    , getConnection
    , allConnections
    , numberOfConnections
    , numberOfRequests
    , prioritizePieces
    , connectionPool
    , matchPieceConnection
    , extractFile
    , getGoodPeers
    ) where

import qualified Data.ByteString     as BS
import qualified Data.Vector         as V
import qualified Data.HashMap.Strict as HM
import qualified Data.Set as Set

import Data.Bits
    ( zeroBits
    , popCount
    , (.&.)
    , (.|.)
    , bit
    , xor
    , testBit
    , setBit
    , complement
    )
import Data.BitField
import Data.Word
import Data.List (find)

import BitTorrent.Protocol
import BitTorrent.Connection

type PiecePool = V.Vector (Maybe BS.ByteString)

extractFile :: PiecePool -> BS.ByteString
extractFile pieces =
    V.foldr step BS.empty pieces
    where
        step ch buf = maybe buf (`BS.append` buf) ch

data StateMachine = StateMachine
    -- | Map of open connections to peers. Indexed by their peer ID.
    { stateMachineConnections     :: HM.HashMap NodeID Connection
    -- | Bitmap of indexes of pieces still needed. 1 bit indicates still
    --   needed.
    , stateMachineNeed            :: BitField
    -- | Bitmap of currently open requests.
    , stateMachineOpenRequests    :: BitField
    -- | A pool of pieces received.
    , stateMachinePiecePool       :: PiecePool
    -- | A list of peers advertised from the tracker.
    , stateMachinePeers           :: Set.Set (Word16, Word32)
    -- | The InfoHash of the file associated with this state machine.
    , stateMachineInfoHash        :: InfoHash
    -- | Peers that we have tried to connect to but appear to be dead
    , stateMachineDeadPeers       :: Set.Set (Word16, Word32)
    } 

-- | Return the list of peers that haven't been marked as dead.
getGoodPeers :: StateMachine -> [(Word16, Word32)]
getGoodPeers StateMachine{..} = 
    filter (not . flip Set.member stateMachineDeadPeers) (Set.toList stateMachinePeers)

getConnection :: NodeID -> StateMachine -> Maybe Connection
getConnection nodeid st = HM.lookup nodeid (stateMachineConnections st)

allConnections :: StateMachine -> [Connection]
allConnections = map snd . HM.toList . stateMachineConnections

initialState :: Int -> InfoHash -> [(Word16, Word32)] -> StateMachine
initialState len infohash peers =
    StateMachine 
        HM.empty 
        zeroBits 
        zeroBits 
        (V.replicate len Nothing) 
        (Set.fromList peers) 
        infohash 
        Set.empty

numberOfConnections :: StateMachine -> Int
numberOfConnections = HM.size . stateMachineConnections

numberOfRequests :: StateMachine -> Int
numberOfRequests = popCount . stateMachineOpenRequests
        
-- | An event that triggers a transition of the state machine.
data Event
    -- | A new connection established to node with peer id. 
    = EvNewConnection NodeID Connection
    -- | The connection to peer id was closed.
    | EvRemoveConnection NodeID
    -- | Attempting to connect to peers, remove from queue.
    | EvConnectPeers [(Word16, Word32)]
    -- | A message was received by the connection to peer id.
    | EvMessageReceived NodeID Message
    -- | A piece has been received.
    | EvNewPiece Word32 Word32 BS.ByteString
    -- | A piece has been requested.
    | EvRequest Word32
    -- | A piece request has been cancelled.
    | EvCancel Word32


-- | Transition the StateMachine to a new state with event
transition :: Event -> StateMachine -> StateMachine
-- | Insert the new connection.
transition (EvNewConnection peerid conn) st@StateMachine{..} = st
    { stateMachineConnections = HM.insert peerid conn stateMachineConnections
    }
-- | Remove the closed connection.
transition (EvRemoveConnection peerid) st@StateMachine{..} = st
    { stateMachineConnections = HM.delete peerid stateMachineConnections
    }
transition (EvConnectPeers peers) st@StateMachine{..} = st
    { stateMachinePeers = foldr Set.delete stateMachinePeers peers
    }
-- | Transition the Connection state associated with peerid.
transition (EvMessageReceived peerid msg) st@StateMachine{..} = st
    { stateMachineConnections = HM.alter updateConnection peerid stateMachineConnections
    }
    where
        updateConnection = maybe Nothing (Just . transitionConnection msg)
-- | Insert the new piece into the piece pool at the specified index.
--   XXX: At the moment, this assumes total pieces are received (it ignores the
--        offset argument).
transition (EvNewPiece idx _ buf) st@StateMachine{..} = st
    { stateMachinePiecePool = stateMachinePiecePool V.// [(fromIntegral idx, Just buf)]
    , stateMachineNeed = stateMachineNeed .&. (complement . bit $ fromIntegral idx)
    }
transition (EvRequest idx) st@StateMachine{..} = st
    { stateMachineOpenRequests = setBit stateMachineOpenRequests (fromIntegral idx)
    }

-- | Calculate a bitfield of the earliest n pieces that are needed but but
--   don't have open requests.
prioritizePieces :: Int -> StateMachine -> BitField
prioritizePieces n st =
    foldr (.|.) zeroBits 
        . map bit 
        . take n 
        . bits 
        $ need `xor` open
        -- ^ Assume there is nothing in open that isn't in need. XORing will
        --   return everything in need that isn't in open.
    where
        need = stateMachineNeed st
        open = stateMachineOpenRequests st

-- | Return a pool of connections that can satisfy requests for pieces in need.
connectionPool :: BitField -> StateMachine -> [Connection]
connectionPool need st =
    filter ((>0) . popCount . (.|.) need . connPieces) (allConnections st)

-- | Match the pieces in need with connections able to satisfy them.
matchPieceConnection :: BitField -> [Connection] -> [(Int, Maybe Connection)]
matchPieceConnection need conns =
    -- XXX: this is not particularly efficient (O(NM) where N is number of
    -- bits, M is number of connections).
    map (\n -> (n,) $ find (canServe n) conns) (bits need)
    where
        canServe :: Int -> Connection -> Bool
        canServe n conn = testBit (connPieces conn) n
