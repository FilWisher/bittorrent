{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ExistentialQuantification #-}

module BitTorrent.Network where

import Control.Monad.Reader

import qualified Network.Socket as NS
import qualified Network.Socket.ByteString as NSB

import Control.Concurrent.MVar
import Control.Concurrent.STM

import qualified Data.Map.Strict as Map
import qualified Data.ByteString as BS
import           Data.Word

import qualified Data.Serialize as S
import           Data.Typeable (Typeable(..), typeOf, typeRepFingerprint)

--import BitTorrent.Monad
--import BitTorrent.StateMachine
--import BitTorrent.Protocol

data ProcessState = ProcessState
    { stateConns :: Map.Map ProcessID NS.Socket
    }
    

data ProcessConfig = ProcessConfig
    { processState   :: MVar ProcessState
    , processID      :: ProcessID
    , processMailbox :: TChan Message
    }

data Address = Address
    { addressIP   :: String
    , addressPort :: String
    }
    deriving (Eq, Ord)

data ProcessID = ProcessID
    { processAddress    :: Address
    , processIdentifier :: Word32
    }
    deriving (Eq, Ord)

-- XXX: Use a set of internal messages to indicate various failures and allow
--      application code to handle it.
data Message =
    RawMessage
    { messageFrom    :: ProcessID
    , messageRawBody :: BS.ByteString
    }
    |
    forall a. S.Serialize a => Message
    { messageFrom :: ProcessID
    , messageBody :: a
    }

-- | InternalMessage are messages sent by the Process internals to indicate
--   different kinds of failure. These are propogated to the process mailbox
--   to defer how failure is handled to the application.
data InternalMessage
    -- | There is no existing connection between the local process and
    --   ProcessID.
    = NoConnection ProcessID

instance S.Serialize InternalMessage where
    put = undefined
    get = undefined

getState :: Process ProcessState
getState = do
    mvar <- processState <$> ask
    liftIO (readMVar mvar)

modifyState :: (ProcessState -> ProcessState) -> Process ()
modifyState fn = do
    st <- processState <$> ask
    liftIO $ modifyMVarMasked_ st (return . fn)

newtype Process a = Process
    { unProcess :: ReaderT ProcessConfig IO a
    }
    deriving
        ( Functor
        , Applicative
        , Monad
        , MonadIO
        , MonadReader ProcessConfig
        )


send :: S.Serialize a => ProcessID -> a -> Process ()
send procid body = do
    conf <- ask
    if procid == processID conf
        then sendLocal procid body
        else sendBuf procid (S.encode body)
    where
        sendLocal :: S.Serialize a => ProcessID -> a -> Process ()
        sendLocal selfid body = do
            mb <- processMailbox <$> ask
            liftIO . atomically $ writeTChan mb (Message selfid body)

        sendBuf :: ProcessID -> BS.ByteString -> Process ()
        sendBuf procid buf = do
            conf <- ask
            st <- getState
            case Map.lookup procid (stateConns st) of
                Nothing -> sendLocal (processID conf) (NoConnection procid)
                Just socket  -> do
                    void . liftIO $ NSB.send socket buf
                    
-- TODO: 
recvWait :: (Typeable a, S.Serialize a) => Process a
recvWait = do
    ch  <- processMailbox <$> ask
    msg <- liftIO $ atomically (readTChan ch)
    case msg of
        RawMessage _ body -> return (S.decode body)
        Message    _ body -> return body
    

{-

    o Multiplex all messages through a single process mailbox.
    o When opening connections to other nodes, spawn threads that monitor
      sockets and put them in the mailbox.
    o When creating connection, generate an identifier and store the connection
      in a HashMap. When a message is received, queue it.


-}
