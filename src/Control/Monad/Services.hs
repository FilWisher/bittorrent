{-# LANGUAGE MultiParamTypeClasses      #-}

module Control.Monad.Services where

import Control.Exception
import qualified Network.Socket as NS
import qualified Data.ByteString as BS

import Data.BitTorrent.Connection
import Data.BitTorrent.Message

data TransportBitTorrentException
    = BitTorrentParseError String
    | BitTorrentIOError String
    deriving (Show)

instance Exception TransportBitTorrentException

class TransportMessage m where
    msgSend :: Message -> m ()
    msgRecv :: m Message

class TransportTCP m where
    connectTCP :: String -> String -> m (Either TransportTCPException NS.Socket)
    sendTCP    :: NS.Socket -> BS.ByteString -> m ()
    recvTCP    :: NS.Socket -> m BS.ByteString

data TransportTCPException
    = TCPIOException IOException
    | TCPUserError String
    deriving (Show)

instance Exception TransportTCPException

class ConnectionManager m where
    createConnection :: NS.Socket -> NodeID -> IO () -> m Connection
    closeConnection  :: Connection -> m ()

