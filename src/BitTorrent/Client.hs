{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module BitTorrent.Client
    ( newClient
    , addTorrent
    , addTorrentFile
    ) where

import Control.Monad.Reader
import Control.Monad.Catch
import Control.Monad.IO.Class

import Control.Concurrent
import Control.Concurrent.Async
import Control.Concurrent.STM

import qualified Network.Socket as NS

import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import           Data.Word
import qualified Data.ByteString.Char8 as B8

import Process

import BitTorrent.Services
import BitTorrent.Action
import BitTorrent.Protocol
import BitTorrent.Monad
import BitTorrent.Torrent
import BitTorrent.Tracker
    ( request
    , createRequest
    , unwrapPeers
    , TrackerRequest
    , TrackerResponse(..)
    , TorrentInfo(..)
    , TorrentFile(..)
    , TrackerEvent(..)
    , TrackerError(..)
    , readTorrentFile
    , DecodeError(..)
    , 
    )

newtype ClientM a = ClientM
    { unClientM :: ReaderT Client IO a
    }
    deriving
        ( Functor
        , Applicative
        , Monad
        , MonadReader Client
        , MonadIO
        , MonadThrow
        )

runClientM :: Client -> ClientM a -> IO a
runClientM cli m = runReaderT (unClientM m) cli

data ClientConfig = ClientConfig
    { clientConfigPort        :: Word16
    , clientConfigThreadCount :: Int
    , clientConfigPeerID      :: Maybe NodeID
    }

data Client = Client
    { clientTorrents   :: TVar (HashMap InfoHash Torrent)
    , clientConfig     :: ClientConfig
    , clientPeerID     :: NodeID
    , clientServer     :: Async ()
    , clientActionChan :: TChan Action
    }

newClient :: MonadIO m => ClientConfig -> m Client
newClient conf = do
    torrents <- liftIO (newTVarIO HM.empty)
    chan <- liftIO $ newTChanIO
    server <- liftIO $ async (createServer chan (clientConfigPort conf))
    peerid <- case clientConfigPeerID conf of
        Just id -> return id
        -- xxx: need to create a real node id here
        Nothing     -> return $ "-FW" <> B8.replicate 17 '0'
    return $ Client 
        { clientTorrents   = torrents
        , clientConfig     = conf
        , clientPeerID     = peerid
        , clientServer     = server
        , clientActionChan = chan
        }

waitAll :: MonadIO m => Client -> m Bool
waitAll _ = return False

torrents :: MonadIO m => Client -> m [Torrent]
torrents client = do 
    torrents <- liftIO . atomically . readTVar $ clientTorrents client
    return $ map snd (HM.toList torrents)

torrent :: MonadIO m => InfoHash -> Client -> m (Maybe Torrent)
torrent ih client = do
    torrents <- liftIO . atomically . readTVar $ clientTorrents client
    return (HM.lookup ih torrents)

insertTorrent :: MonadIO m => Torrent -> Client -> m ()
insertTorrent torrent client = liftIO . atomically $
    modifyTVar' torrents (insert torrent)
    where
        torrents = clientTorrents client
        insert t = HM.insert (torrentInfoHash t) t

addTorrent :: TorrentFile -> ClientM ()
addTorrent file = do
    torrent <- torrentFile file
    insertTorrent torrent =<< ask

addTorrentFromFile :: FilePath -> ClientM ()
addTorrentFromFile filepath = do
    efile <- liftIO $ readTorrentFile filepath
    case efile of
        Left err -> throwM (DecodeError err)
        Right file -> addTorrent file

torrentFile :: (MonadReader Client m, MonadThrow m, MonadIO m) => TorrentFile -> m Torrent
torrentFile torrentfile = do
    client <- ask
    eresp <- liftIO $ request (trackerReq client)
    resp <- case eresp of
        Left err -> throwM err
        Right (TrackerResponseError err) -> throwM (TrackerError err)
        Right r -> return r

    threads <- liftIO $ runBitTorrentM
        (length $ infoPieces $ torrentFileInfo torrentfile)
        (torrentFileInfoHash torrentfile)
        (unwrapPeers $ responsePeers resp)
        (spawn $ clientConfigThreadCount $ clientConfig client)

    return Torrent
        { torrentFilePath = Nothing
        , torrentInfoHash = torrentFileInfoHash torrentfile
        , torrentTorrentFile = torrentfile
        , torrentThreads = threads
        }
    where
        trackerReq :: Client -> TrackerRequest
        trackerReq client = 
            createRequest 
                (clientPeerID client) 
                (Just Started) 
                (clientConfigPort $ clientConfig client)
                torrentfile

        spawn :: Int -> BitTorrentM [Async ()]
        spawn n = do
            t <- forkM ticker
            ts <- replicateM n (forkM loop)
            return (t:ts)

        ticker :: BitTorrentM ()
        ticker = do
            ch <- configChan <$> ask
            forever $ do
                liftIO $ threadDelay 5000000
                liftIO . atomically $ writeTChan ch Tick

        loop :: BitTorrentM ()
        loop = do
            tid <- liftIO (myThreadId)
            ch <- configChan <$> ask
            action <- liftIO . atomically $ readTChan ch
            update action
            loop

createServer :: MonadIO m => TChan Action -> Word16 -> m ()
createServer chan port = do
    socket <- liftIO $ open =<< resolve (show port)
    loop socket
    where
        resolve port = do
            let hints = NS.defaultHints
                  { NS.addrFlags = [NS.AI_PASSIVE]
                  , NS.addrSocketType = NS.Stream
                  }
            addr:_ <- NS.getAddrInfo (Just hints) Nothing (Just port)
            return addr

        open addr = do
            socket <- NS.socket (NS.addrFamily addr) (NS.addrSocketType addr) (NS.addrProtocol addr)
            NS.setSocketOption socket NS.ReuseAddr 1
            NS.bind socket (NS.addrAddress addr)
            NS.listen socket 10
            return socket

        loop sock = forever $ do
            (conn, peer) <- liftIO $ NS.accept sock
            liftIO $ async (handleConnection conn)

        handleConnection socket =
            liftIO . atomically . writeTChan chan $ IncomingSocket socket
