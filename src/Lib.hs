{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Lib where

import Network.HTTP.Types (urlEncode)

import Control.Concurrent
import Control.Concurrent.STM
import Control.Concurrent.Async

import Control.Applicative (liftA2)

import Crypto.Hash

import Control.Monad
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Catch
import Control.Monad.Logger
import Control.Monad.IO.Class

import           Text.Read (readEither)
import qualified Data.Text as T
import           Data.Word
import           Data.Bencode
import           Data.Bits
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString as BS
import qualified Data.Map.Strict as Map
import qualified Data.HashMap.Strict as HM
import qualified Data.Serialize as S
import           Data.ByteArray (convert)

import qualified Network.Socket as NS
import Network.Socket (hostAddressToTuple)

import Process

import BitTorrent.StateMachine
import BitTorrent.Protocol
import BitTorrent.Monad
import BitTorrent.Action
import BitTorrent.Services

import System.Random

import BitTorrent.Tracker

randomNodeID :: IO NodeID
randomNodeID = BS.pack <$> replicateM 20 randomIO

getLength :: Value -> IO Int
getLength (Dictionary d) = case Map.lookup (String "files") d of
    Nothing -> error "No files key"
    Just (List files) -> undefined

-- XXX: Need to query the tracker and parse the peers out of the response.
getPeers :: Value -> IO [(Word16, Word32)]
getPeers _ = return []
    -- case Map.lookup (String "peers") d of
    --     Nothing -> error "No peers key"
    --     Just (String v) -> flip S.runGet v $ do
    --         S.getWord32be
    --     Just _ -> error "Unsupported peers format"

getInfoHash :: Value -> IO InfoHash
getInfoHash (Dictionary d) = case Map.lookup (String "info") d of
    Nothing -> error "No info key"
    Just v  -> return (encode v)

getPieces :: Value -> IO [BS.ByteString]
getPieces _ = return []

nThreads :: Int
nThreads = 5

torrentFile :: FilePath -> IO [Async ()]
torrentFile path = do
    let node = ("-FW" <> B8.replicate 17 '0')
    print (BS.length node)
    let key = "123456789"
    efile <- readTorrentFile path
    file <- case efile of
        Left err -> error err
        Right f -> return f

    let req = createRequest node key (Just Started) 6881 file
    eresp <- request req
    resp <- case eresp of
        Left err -> error err
        Right (TrackerResponseError err) -> error (B8.unpack err)
        Right r -> return r

    runBitTorrentM
        (length $ infoPieces $ torrentInfo file)
        (torrentInfoHash file)
        (unwrapPeers $ responsePeers resp)
        (spawn nThreads)

    where
        spawn :: Int -> BitTorrentM [Async ()]
        spawn n = do
            t <- forkM ticker
            s <- forkM (createServer "6881")
            ts <- replicateM n (forkM loop)
            return (s:t:ts)

        ticker :: BitTorrentM ()
        ticker = do
            ch <- configChan <$> ask
            forever $ do
                liftIO $ threadDelay 5000000
                logDebugN "Creating Tick"
                liftIO . atomically $ writeTChan ch Tick

        loop :: BitTorrentM ()
        loop = do
            tid <- liftIO (myThreadId)
            ch <- configChan <$> ask
            action <- liftIO . atomically $ readTChan ch
            logDebugN . T.pack $ show tid <> ": " <> show action
            logDebugN . T.pack $ show tid <> ": LOOP"
            update action
            st <- stateMachineConnections <$> get
            loop


cancelAll :: [Async a] -> IO ()
cancelAll = mapM_ cancel

createServer :: String -> BitTorrentM ()
createServer port = do
    socket <- liftIO $ open =<< resolve port
    loop socket
    where
        resolve :: String -> IO NS.AddrInfo
        resolve port = do
            let hints = NS.defaultHints
                  { NS.addrFlags = [NS.AI_PASSIVE]
                  , NS.addrSocketType = NS.Stream
                  }
            addr:_ <- NS.getAddrInfo (Just hints) Nothing (Just port)
            return addr
        open :: NS.AddrInfo -> IO NS.Socket
        open addr = do
            socket <- NS.socket (NS.addrFamily addr) (NS.addrSocketType addr) (NS.addrProtocol addr)
            NS.setSocketOption socket NS.ReuseAddr 1
            NS.bind socket (NS.addrAddress addr)
            NS.listen socket 10
            return socket
        loop :: NS.Socket -> BitTorrentM ()
        loop sock = forever $ do
            (conn, peer) <- liftIO $ NS.accept sock
            forkM (handleConnection conn)

        handleConnection :: NS.Socket -> BitTorrentM ()
        handleConnection socket = do
            logDebugN "Received connection"
            emit (IncomingSocket socket)

    
--    -- d <- decode <$> BS.readFile "/home/william/src/bittorrent/test.ben"
--    case d of
--        Left err -> print err
--        Right (Dictionary d) -> case lookup (String "info") d of
--            Nothing -> print "No info"
--            Just (Dictionary i) -> case lookup (String "pieces") i of
--                Nothing -> print "No pieces"
--                Just v -> print (length <$> parsePieces v)
--    --case d of
--    --    Left err -> print err
--    --    Right (Dictionary d) -> case lookup (String "peers") d of
--    --        Nothing -> print "No peers"
--    --        Just v  -> print (map transform <$> parsePeerAddrs v)
--    where
--        transform (ip,port) = (hostAddressToTuple ip, port)


-- Order of operation:
-- o parse torrent file
-- o request peers from announce url
-- o connect to peer(s) ip:port (just one for now)
-- o start the bittorrent protocol handshake
-- o implement bittorrent full protocol:
--      o Bitfield
--      o Have
--      o Choke/Unchoke
--      o Interested/Not interested
--      o Request
--      o Piece
--      o Cancel
--      o Port
-- o Reassemble file
--
