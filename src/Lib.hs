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
import qualified Data.Serialize as S
import           Data.ByteArray (convert)

import Network.Socket (hostAddressToTuple)

import BitTorrent.Protocol
import BitTorrent.Monad
import BitTorrent.Action

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

someFunc :: IO ()
someFunc = do
    einfo <- decode <$> BS.readFile "/home/william/src/bittorrent/slackware.torrent"
    print einfo
    case einfo of
        Left err -> print einfo
        Right info -> do
            peers <- getPeers info
            infohash <- getInfoHash info
            pieces <- getPieces info
            ch <- newTChanIO
            runBitTorrentM (length pieces) infohash peers ch $ do
                liftIO $ async $ forever $ do
                    threadDelay 1000000
                    atomically $ writeTChan ch Tick
                loop ch

loop :: TChan Action -> BitTorrentM ()
loop ch = do
    logDebugN "HI"
    action <- liftIO . atomically $ readTChan ch
    logDebugN . T.pack $ show action
    update action
    loop ch


torrentFile :: FilePath -> IO ()
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

    ch <- newTChanIO

    -- Start the ticker (every 1 second)
    liftIO $ async $ forever $ do
        threadDelay 1000000
        atomically $ writeTChan ch Tick

    runBitTorrentM
        (length $ infoPieces $ torrentInfo file)
        (torrentInfoHash file)
        (unwrapPeers $ responsePeers resp)
        ch
        (loop ch)

    where
        loop :: TChan Action -> BitTorrentM ()
        loop ch = do
            action <- liftIO . atomically $ readTChan ch
            logDebugN . T.pack $ show action
            update action
            loop ch



    
    
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
