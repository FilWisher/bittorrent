{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module BitTorrent.Tracker where

import Control.Applicative (liftA2)
import Control.Exception

import Network.Socket
import Network.HTTP.Client
import Network.HTTP.Client.TLS

import Crypto.Hash
import Data.ByteArray

import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString as BS

import qualified Data.Serialize as S
import qualified Data.Serialize.Get as S

import BitTorrent.Protocol
import Data.Bencode
import Data.Bits
import Data.Word
import Text.Read (readEither)
import Network.HTTP.Types (urlEncode)

data DecodeError = DecodeError String deriving (Show)
instance Exception DecodeError

request :: TrackerRequest -> IO (Either DecodeError TrackerResponse)
request tr = do
    mgr <- newManager tlsManagerSettings
    req <- parseRequest (B8.unpack $ formatTrackerRequest tr)
    res <- httpLbs req mgr
    return (decodeBody res)
    where
        decodeBody =
            either (Left . DecodeError) Right 
                . (fromBencode =<<) 
                . decode 
                . BSL.toStrict 
                . responseBody

-- Split a ByteString into a stream of ByteStrings of length `n`
chunksOf :: Int -> BS.ByteString -> [BS.ByteString]
chunksOf n bs
    | BS.null bs = []
    | otherwise = BS.take n bs:chunksOf n (BS.drop n bs)

data TorrentInfo = TorrentInfo
   { infoPieceLength :: Int
   , infoPieces      :: [BS.ByteString]
   , infoPrivate     :: Maybe Int
   , infoFile :: InfoFile
   }
    deriving (Show)

instance FromBencode TorrentInfo where
    fromBencode o =
        TorrentInfo
            <$> o .: "piece length"
            <*> (chunksOf 20 <$> o .: "pieces")
            <*> o .:? "private"
            <*> fromBencode o

data File = File
    { fileLength :: Int
    , fileMD5    :: Maybe BS.ByteString
    , path       :: [BS.ByteString]
    }
    deriving (Show)

instance FromBencode File where
    fromBencode o =
        File
            <$> o .: "length"
            <*> o .:? "md5sum"
            <*> o .: "path"

data InfoFile
    = SingleFile
        { infoFileName     :: BS.ByteString
        , singleFileLength :: Int
        , singleFileMD5    :: Maybe BS.ByteString
        }
    | MultiFile
        { infoFileName   :: BS.ByteString
        , multiFileFiles :: [File]
        }
    deriving (Show)

instance FromBencode InfoFile where
    fromBencode o = o .: "files" >>= parseFile
        where
            parseFile :: Value -> Either String InfoFile
            parseFile files = case files of
                -- | A list formated `files` key indicates multi-file mode.
                List _ ->
                    MultiFile 
                        <$> o .: "name" 
                        <*> o .: "files"
                -- | Otherwise, assume single-file mode.
                _      ->
                    SingleFile
                        <$> o .: "name" 
                        <*> o .: "length" 
                        <*> o .:? "md5sum"

testid :: BS.ByteString
testid = "-TZ-0000-00000000000"

data TrackerEvent = Started | Stopped | Completed

instance Show TrackerEvent where
    show Started   = "started"
    show Stopped   = "stopped"
    show Completed = "completed"

data TrackerRequest = TrackerRequest
    { requestURL        :: BS.ByteString
    , requestInfoHash   :: BS.ByteString
    , requestLeft       :: Int
    , requestPeerID     :: NodeID
    , requestUploaded   :: Int
    , requestDownloaded :: Int
    , requestEvent      :: Maybe TrackerEvent
    -- , requestKey        :: BS.ByteString
    , requestPort       :: Word16
    }
    deriving (Show)

-- | Format a tracker request as a URL.
formatTrackerRequest :: TrackerRequest -> BS.ByteString
formatTrackerRequest TrackerRequest{..} = requestURL <> "?" <> BS.intercalate "&"
    [ "info_hash"  .> urlEncode False requestInfoHash
    , "peer_id"    .> requestPeerID
    , "left"       .> showB8 requestLeft
    , "uploaded"   .> showB8 requestUploaded
    , "downloaded" .> showB8 requestDownloaded
    , "event"      .>? (showB8 <$> requestEvent)
    -- , "key"        .> requestKey
    , "port"       .> showB8 requestPort
    ]
    where
        -- XXX: Unsure how this will be handled: it could cause errors when
        -- requesting without a specified event.
        (.>?) :: BS.ByteString -> Maybe BS.ByteString -> BS.ByteString
        (.>?) key (Just value) = key <> "=" <> value
        (.>?) key Nothing = key

        (.>) :: BS.ByteString -> BS.ByteString -> BS.ByteString
        (.>) key value = key <> "=" <> value

        showB8 :: Show a => a -> BS.ByteString
        showB8 = B8.pack . show

data TrackerError = TrackerError BS.ByteString deriving (Show)
instance Exception TrackerError

data TrackerResponse
    = TrackerResponseError
        { responseFailureReason :: BS.ByteString
        }
    | TrackerResponseSuccess
        { responseWarningMessage :: Maybe BS.ByteString
        , responseInterval       :: Int
        , responseMinInterval    :: Maybe Int
        , responseTrackerID      :: Maybe BS.ByteString
        , responseComplete       :: Maybe Int
        , responseIncomplete     :: Maybe Int
        , responsePeers          :: Peers
        }
    deriving (Show)

instance FromBencode TrackerResponse where
    fromBencode o =
        case o .: "failure reason" of
            -- | If a `failure reason` key is present, assume failure.
            Right err -> return $ TrackerResponseError err
            -- | Otherwise, assume an success.
            Left _ ->
                TrackerResponseSuccess
                    <$> o .:? "warning message"
                    <*> o .: "interval"
                    <*> o .:? "min interval"
                    <*> o .:? "tracker id"
                    <*> o .:? "complete"
                    <*> o .:? "incomplete"
                    <*> o .: "peers"

-- XXX: For the moment, just support IPv4 over the binary model
data PeerAddr = PeerAddr Word32 Word16 
    deriving (Show)
data Peers = Peers [PeerAddr]
    deriving (Show)

unwrapPeers :: Peers -> [(Word16, Word32)]
unwrapPeers (Peers addrs) = map unwrap addrs
    where
        unwrap (PeerAddr ip port) = (port,ip)

many :: S.Get a -> S.Get [a]
many getter = do
    v <- getter
    empty <- S.isEmpty
    if empty
        then return [v]
        else (v:) <$> many getter

instance FromBencode Peers where
    fromBencode o =
        case o of
            String bs ->
                Peers <$> S.runGet (many getPeerAddr) bs
            List vs -> Peers <$> traverse toPeer vs
            _ -> Left "Unsupported `peers` format: number"
        where
            getPeerAddr :: S.Get PeerAddr
            getPeerAddr = liftA2 PeerAddr S.getWord32be S.getWord16be

            parseIP :: Value -> Either String Word32
            parseIP (String bs) = do
                bytes <- traverse readEither $ map B8.unpack $ BS.split 46 bs
                case bytes of
                    [d,c,b,a] -> Right (a `shiftL` 24 .|. b `shiftL` 16 .|. c `shiftL` 8 .|. d)
                    _ -> Left "Incorrect IPv4 format"
            parseIP _ = Left "Unrecognized IP format"

            toPeer p@(Dictionary d) =
                PeerAddr
                    <$> (parseIP =<<  p .: "ip")
                    <*> (fromIntegral <$> (p .: "port" :: Either String Int))
            toPeer _ = Left "Unsupported `peer` format"


sha1 :: BS.ByteString -> BS.ByteString
sha1 = convert . (hash :: (BS.ByteString -> Digest SHA1))

data TorrentFile = TorrentFile
    { torrentFileInfo     :: TorrentInfo
    , torrentFileInfoHash :: BS.ByteString
    , torrentFileAnnounce :: BS.ByteString
    }
    deriving (Show)

-- | Create a TrackerRequest request from a TorrentFile
createRequest :: NodeID -> Maybe TrackerEvent -> Word16 -> TorrentFile -> TrackerRequest
createRequest peerid ev port TorrentFile{..} = TrackerRequest
    { requestURL = torrentFileAnnounce
    , requestInfoHash = torrentFileInfoHash
    , requestLeft = calculateLength (infoFile torrentFileInfo)
    , requestPeerID = peerid
    , requestUploaded = 0
    , requestDownloaded = 0
    , requestEvent = ev
    -- , requestKey = key
    , requestPort = port
    }
    where
        calculateLength f@SingleFile{} = singleFileLength f
        calculateLength f@MultiFile{} = foldr ((+) . fileLength) 0 (multiFileFiles f)

instance FromBencode TorrentFile where
    fromBencode o =
        TorrentFile
            <$> o .: "info"
            <*> (sha1 . encode <$> o .: "info")
            <*> o .: "announce"

readTorrentFile :: FilePath -> IO (Either String TorrentFile)
readTorrentFile path = unbencode <$> BS.readFile path

slackware :: FilePath
slackware = "/home/william/src/bittorrent/slackware.torrent"

test :: IO (Either String TorrentFile)
test = readTorrentFile slackware

test1 :: IO (Either String TrackerResponse)
test1 = unbencode <$> BS.readFile "/home/william/src/bittorrent/tracker.response"

testRequest :: TrackerRequest
testRequest = TrackerRequest
    { requestURL        = "http://trackers.transamrit.net:8082/announce"
    , requestInfoHash   = "%D0B%DBC5%A8%C8%88M%A0@%E3%ABM%C2%DFl%3E%FC%EA"
    , requestLeft       = 697372140
    , requestPeerID     = "-TZ-0000-00000000000"
    , requestUploaded   = 0
    , requestDownloaded = 0
    , requestEvent      = Just Started
    -- , requestKey        = "192837465"
    , requestPort       = 6881
    }


