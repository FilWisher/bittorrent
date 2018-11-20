{-# LANGUAGE MultiParamTypeClasses #-}

module BitTorrent.Services where

import Control.Monad.Reader
import Control.Monad.IO.Class

import qualified Data.ByteString as BS
import qualified Network.Socket  as NS

import qualified Data.Serialize as S 

import Control.Concurrent.Async
import Control.Concurrent.STM
import BitTorrent.Action

class TCPCommunicator m where
    sendTCP :: NS.Socket -> BS.ByteString -> m ()
    recvTCP :: NS.Socket -> m BS.ByteString

class HasActionChan a where
    getActionChan :: a -> TChan Action

emit :: (MonadIO m, MonadReader a m, HasActionChan a) => Action -> m ()
emit action = do
    ch <- getActionChan <$> ask
    liftIO (atomically $ writeTChan ch action)

