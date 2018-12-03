{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module BitTorrent.Torrent where

import Control.Monad.Reader
import Control.Monad.Catch
import Control.Monad.IO.Class

import Control.Concurrent.Async

import BitTorrent.Protocol
import BitTorrent.Tracker (TorrentFile(..))

import qualified Data.ByteString as BS
import           Data.Word

data Peer = Peer
    { peerID   :: NodeID
    , peerIP   :: BS.ByteString
    , peerPort :: Word16
    }
    deriving (Show)

newtype TorrentM a = TorrentM
    { unTorrentM :: ReaderT Torrent IO a
    }
    deriving
        ( Functor
        , Applicative
        , Monad
        , MonadReader Torrent
        , MonadIO
        , MonadThrow
        )

data Torrent = Torrent
    { torrentFilePath    :: Maybe FilePath
    , torrentInfoHash    :: InfoHash
    , torrentTorrentFile :: TorrentFile
    , torrentThreads     :: [Async ()]
    }

addPeers :: [Peer] -> TorrentM ()
addPeers = mapM_ addPeer

-- | Add peer to list of peers. Try to connect if peers in need.
addPeer :: Peer -> TorrentM ()
addPeer peer = return ()

closeTorrent :: MonadIO m => Torrent -> m () 
closeTorrent torrent = liftIO . mapM_ cancel $ torrentThreads torrent

-- | Update the list of trackers a torrent knows about
addTrackers :: [String] -> TorrentM ()
addTrackers trackers = return ()

bytesComplete :: TorrentM Word64
bytesComplete = return 0

bytesMissing :: TorrentM Word64
bytesMissing = return 0

cancelPieces :: Int -> Int -> TorrentM ()
cancelPieces begin end = return ()

-- | Prioritize downloading a specific piece
downloadPiece :: Int -> TorrentM ()
downloadPiece n = return ()

-- | Prioritize downloading pieces in the range [begin, end).
downloadPieces :: Int -> Int -> TorrentM ()
downloadPieces begin end = mapM_ downloadPiece [begin..end]

-- | Mark all the pieces for download
downloadAll :: TorrentM ()
downloadAll = return ()
