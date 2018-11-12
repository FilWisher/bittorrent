{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module BitTorrent.Connection.Monad
    ( ConnectionM
    , runConnectionM
    ) where

import Control.Monad.Reader
import Control.Monad.Catch
import Control.Monad.IO.Class

import qualified Network.Socket as NS
import qualified Network.Socket.ByteString as NSB

import qualified Data.BitTorrent.Message as Message
import           Data.BitTorrent.Message (Message)
import           Data.BitTorrent.Connection
import           Data.BitTorrent.Protocol

import Control.Monad.Services
import Control.Concurrent.STM

data Config = Config
    { configSocket :: NS.Socket
    , configChannel :: TChan Action
    }

newtype ConnectionM a = ConnectionM
    { unConnection :: ReaderT Config IO a
    }
    deriving
        ( Functor
        , Applicative
        , Monad
        , MonadReader Config
        , MonadThrow
        , MonadIO
        )

runConnectionM :: NS.Socket -> TChan Action -> ConnectionM a -> IO a
runConnectionM sock ch m = runReaderT (unConnection m) (Config sock ch) 

instance TransportMessage ConnectionM where
    msgSend msg = do
        socket <- configSocket <$> ask
        void . liftIO $ NSB.send socket (Message.encode msg)
    msgRecv = do
        socket <- configSocket <$> ask
        buf <- liftIO $ Message.decode <$> NSB.recv socket 1024
        case buf of
            Left err  -> throwM $ BitTorrentParseError err
            Right msg -> return msg

instance EventEmitter ConnectionM Action where
    emit ev = do
        ch <- configChannel <$> ask
        liftIO $ atomically (writeTChan ch ev)
