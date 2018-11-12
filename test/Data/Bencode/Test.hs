module Data.Bencode.Test where

import Control.Monad (replicateM)

import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Arbitrary

import qualified Data.ByteString as BS

import qualified Data.Bencode as Bencode
import           Data.Bencode (Value)

instance Arbitrary Value where
    arbitrary = oneof
        [ string
        , Bencode.Number     <$> arbitrary
        -- Value is a recursive type so generating lists of Values of arbitrary
        -- size can take a very long time.
        , Bencode.List       <$> list (0,3) arbitrary
        , Bencode.Dictionary <$> list (0,3) ((,) <$> string <*> arbitrary)
        ]
        where
            string :: Gen Value
            string = Bencode.String . BS.pack <$> arbitrary

            list :: (Int, Int) -> Gen a -> Gen [a]
            list range gen = choose range >>= flip replicateM gen

tests :: Spec
tests =
    describe "Data.Bencode" $ do
        it "decode . encode == id" $
            property $ \v ->
                either (const False) (==v) $
                    Bencode.decode (Bencode.encode v)
