module BitTorrent.Main where

import Control.Monad.IO.Class
import Control.Monad (forever)

import Control.Concurrent
import Control.Concurrent.Async
import Control.Concurrent.STM

import BitTorrent.Monad
import BitTorrent.Action

main :: IO ()
main = do
    ch <- newTChanIO

    async (tickThread 1000000 ch)
    runBitTorrentM ch $ forever $ do
        act <- liftIO $ atomically (readTChan ch)
        update act
    where
        tickThread :: Int -> TChan Action -> IO ()
        tickThread duration ch = forever $ do
            threadDelay duration
            atomically (writeTChan ch Tick)

