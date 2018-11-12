{-# LANGUAGE OverloadedStrings #-}

module BitTorrent.Protocol
    ( NodeID
    , InfoHash
    , Message(..)
    , Handshake(..)
    ) where

import Control.Monad (guard)

import qualified Data.ByteString as BS
import qualified Data.Serialize as S
import           Data.Serialize (Serialize)

import           Data.Word
import qualified Data.BitField as BitField

type NodeID   = BS.ByteString
type InfoHash = BS.ByteString

data Handshake = Handshake InfoHash NodeID

data Message
    = KeepAlive
    | Choke
    | Unchoke
    | Interested
    | NotInterested
    | Have Word32
    | BitField BitField.BitField
    | Request Word32 Word32 Word32
    | Piece Word32 Word32 BS.ByteString
    | Cancel Word32 Word32 Word32
    | Port Int
    deriving (Show, Eq)

messageLength :: Message -> Int
messageLength KeepAlive               = 0
messageLength Choke                   = 1
messageLength Unchoke                 = 1
messageLength Interested              = 1
messageLength NotInterested           = 1
messageLength (Have piece)            = 5
messageLength (BitField bits)         = 1 + BitField.length bits
messageLength (Request index off len) = 13
messageLength (Piece index off block) = 9 + BS.length block
messageLength (Cancel index off len)  = 13
messageLength (Port port)             = 3

instance S.Serialize Message where
    put m = do
        S.putWord32be (fromIntegral $ messageLength m)
        case m of
            KeepAlive     -> return ()
            Choke         -> S.putWord8 0
            Unchoke       -> S.putWord8 1
            Interested    -> S.putWord8 2
            NotInterested -> S.putWord8 3
            Have piece -> do
                S.putWord8 4
                S.putWord32be piece
            BitField bits -> do
                S.putWord8 5 
                S.putByteString (BitField.unwrap bits)
            Request index off len -> do
                S.putWord8 6
                S.putWord32be index
                S.putWord32be off
                S.putWord32be len
            Piece index off block -> do
                S.putWord8 7 
                S.putWord32be index
                S.putWord32be off
                S.putByteString block
            Cancel index off len -> do
                S.putWord8 8 
                S.putWord32be index
                S.putWord32be off
                S.putWord32be len
            Port port -> do
                S.putWord8 9 
                S.putWord16be (fromIntegral port)
    get = do
        S.getWord32be
        empty <- S.isEmpty
        if empty
            then return KeepAlive
            else do
                t <- S.getWord8
                case t of
                    0 -> return Choke
                    1 -> return Unchoke
                    2 -> return Interested
                    3 -> return NotInterested
                    4 -> Have <$> S.get
                    5 -> BitField . BitField.BitField <$> (S.remaining >>= S.getByteString)
                    6 -> Request <$> S.get <*> S.get <*> S.get
                    7 -> Piece <$> S.get <*> S.get <*> (S.remaining >>= S.getByteString)
                    8 -> Cancel <$> S.get <*> S.get <*> S.get
                    9 -> Port . fromIntegral <$> S.getWord16be
                    _ -> fail "Unexpected message"

protocol :: BS.ByteString
protocol = "BitTorrent protocol"

instance Serialize Handshake where
    put (Handshake infohash peerid) = do
        S.putByteString protocol
        S.putByteString infohash
        S.putByteString peerid
    get = do
        str <- S.getByteString (BS.length protocol)
        guard (str == protocol)
        Handshake <$> S.getByteString 20 <*> S.getByteString 20

