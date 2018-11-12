{-# LANGUAGE MultiParamTypeClasses #-}

module BitTorrent.Services where

import qualified Data.ByteString as BS
import qualified Network.Socket  as NS

import qualified Data.Serialize as S 

import Control.Concurrent.Async
import Control.Concurrent.STM
import BitTorrent.Action

-- Emit events of type a
class EventEmitter a m where
    emit :: a -> m ()

class TCPCommunicator m where
    sendTCP :: NS.Socket -> BS.ByteString -> m ()
    recvTCP :: NS.Socket -> m BS.ByteString
