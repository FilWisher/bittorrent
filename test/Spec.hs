module Main where

import Control.Monad (mapM_)
import Test.Hspec

import qualified Data.Bencode.Test
import qualified Data.BitField.Test
import qualified Data.BitTorrent.Message.Test
import qualified Data.BitTorrent.Actions.Test

main :: IO ()
main = hspec $ do
    Data.Bencode.Test.tests
    Data.BitField.Test.tests
    Data.BitTorrent.Message.Test.tests
    Data.BitTorrent.Actions.Test.tests
