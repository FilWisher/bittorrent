{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}

module Data.BitTorrent.Actions.Test where

import Test.Hspec
import Test.QuickCheck

import Control.Monad.Reader
import Control.Monad.Catch
import Control.Monad.Identity
import Control.Monad.IO.Class

import Control.Concurrent.Async
import Control.Concurrent

import Control.Monad.Services
import Data.BitTorrent.ActionInterpreter
import Data.BitTorrent.Actions
import Data.BitTorrent.Connection
import Data.BitTorrent.Message (Message(..))
import qualified Data.BitTorrent.Message as Message

import qualified Network.Socket as NS
import qualified Network.Socket.ByteString as NSB

import System.IO
import System.Directory

import Data.Bits
import Data.IORef
import qualified Data.ByteString as BS

data TestState = TestState
    { testSocket     :: NS.Socket
    , testSocketPath :: FilePath
    , action         :: IO ()
    }

newtype TestInterpreter a = TestInterpreter
    { unTestInterpreter :: ReaderT TestState IO a
    }
    deriving
        ( Functor
        , Applicative
        , Monad
        , MonadIO
        , MonadThrow
        , MonadReader TestState
        )

runTestInterpreter :: TestState -> TestInterpreter a -> IO a
runTestInterpreter conf interpreter =
    runReaderT (unTestInterpreter interpreter) conf

-- runTestInterpreter :: TestInt
-- XXX: Use TestInterpreter to test the action handlers
-- XXX: Review whether ConnectionManager is really a good idea

instance ConnectionManager TestInterpreter where
    createConnection sock nodeId fn = do
        st <- ask
        act <- liftIO $ async (action st)
        return $ Connection
            { _connPieces = zeroBits
            , _connSelf   = initNode
            , _connPeer   = initNode
            , _connSocket = sock
            , _connThread = act
            , _connId     = nodeId
            }
    closeConnection conn = liftIO $ do
        NS.close (_connSocket conn)
        cancel (_connThread conn)

testState :: FilePath -> IO TestState
testState path = do
    sock <- NS.socket NS.AF_UNIX NS.Stream 0
    NS.bind sock (NS.SockAddrUnix path)
    liftIO $ NS.setSocketOption sock NS.ReuseAddr 1
    NS.listen sock 1 
    return $ TestState sock path (return ())

instance TransportTCP TestInterpreter where
    connectTCP _ _ = do
        path <- testSocketPath <$> ask
        sock <- liftIO $ NS.socket NS.AF_UNIX NS.Stream 0
        liftIO $ NS.connect sock (NS.SockAddrUnix path)
        return . Right $ sock
    sendTCP sock buf = void $ liftIO (NSB.send sock buf)
    recvTCP sock = liftIO (NSB.recv sock 1024)

-- handshake
--     :: ( MonadThrow m
--        , TransportTCP m
--        , MonadReader Config m
--        )
--     => InfoHash -> NS.Socket -> m NodeID

newtype TestIO a = TestIO (IO a)
    deriving
        ( Functor
        , Applicative
        , Monad
        , MonadThrow
        , MonadIO
        )
runTestIO (TestIO act) = act

instance TransportTCP TestIO where
    connectTCP path _ = TestIO $ do
        socket <- NS.socket NS.AF_UNIX NS.Stream 0
        NS.connect socket (NS.SockAddrUnix path)
        return (Right socket)
    sendTCP sock bs = TestIO (void $ NSB.send sock bs)
    recvTCP sock    = TestIO (NSB.recv sock 4096)

unixsocket :: IO NS.Socket
unixsocket = NS.socket NS.AF_UNIX NS.Stream 0

listenunix :: FilePath -> IO NS.Socket
listenunix path = do
    socket <- unixsocket
    NS.bind socket (NS.SockAddrUnix path)
    NS.listen socket 1
    return socket

connectunix :: FilePath -> IO NS.Socket
connectunix path = do
    socket <- unixsocket
    NS.connect socket (NS.SockAddrUnix path)
    return socket

tests :: Spec
tests =
    -- XXX: This test seems unpleasant. Is this right?
    describe "Data.BitTorrent.Actions" $ do
        it "handshake exchange" $ do
            infohash <- generate randomBytestring
            serverid <- generate randomBytestring
            peerid <- generate randomBytestring
            removeFile socketpath
            sock <- listenunix socketpath
            tid <- forkIO $ do
                (cli, _) <- liftIO $ NS.accept sock
                runTestIO $ do
                    hs <- Message.decodeHandshake <$> recvTCP cli
                    case hs of
                        Left err -> throwM (BitTorrentParseError err)
                        Right (Message.Handshake receivedHash receivedId) ->
                            sendTCP cli (Message.encodeHandshake $ Message.Handshake receivedHash serverid)
            cli <- connectunix socketpath
            hs <- runTestIO $ handshake cli (Message.Handshake infohash peerid)
            hs `shouldBe` Message.Handshake infohash serverid
    where
        socketpath = "/tmp/bittorrent.sock"
        randomBytestring = BS.pack <$> replicateM 20 arbitrary
