module Data.BitField.Test where

import Test.Hspec
import Test.QuickCheck (choose, Gen, property, forAll, suchThat)
import Test.QuickCheck.Arbitrary

import           Data.BitField (BitField)
import qualified Data.BitField as BitField

import           Data.ByteString (ByteString)
import qualified Data.ByteString as BS

import Data.Bits ((.|.), (.&.), xor, complement, bit, testBit)
import Data.Word
import Control.Monad (replicateM)

bytes :: Int -> Gen [Word8]
bytes n = replicateM n arbitrary

bitfield :: [Word8] -> BitField
bitfield = BitField.BitField . BS.pack

data EqualLists a = EqualLists [a] [a]
    deriving (Show)

instance Arbitrary a => Arbitrary (EqualLists a) where
    arbitrary = choose (0, 20) >>= \n -> 
        EqualLists 
            <$> replicateM n arbitrary 
            <*> replicateM n arbitrary

nat :: Gen Int
nat = (arbitrary :: Gen Int) `suchThat` (> 0)

tests :: Spec
tests =
    describe "Data.BitField" $ do
        it "(BitField as) xor (BitField bs) == Bitfield (zipWith xor as bs)" $
            property $ \(EqualLists as bs) ->
                bitfield as `xor` bitfield bs == bitfield (zipWith xor as bs)
        it "(BitField as) .|. (BitField bs) == BitField (zipWith (.|.) as bs)" $
            property $ \(EqualLists as bs) ->
                bitfield as .|. bitfield bs == bitfield (zipWith (.|.) as bs)
        it "(BitField as) .&. (BitField bs) == BitField (zipWith (.&.) as bs)" $
            property $ \(EqualLists as bs) ->
                bitfield as .&. bitfield bs == bitfield (zipWith (.&.) as bs)
        it "complement (BitField as) == BitField (map complement as)" $
            property $ \as ->
                complement (bitfield as) == bitfield (map complement as)
        it "testBit (bit n) n == True" $
            forAll nat $ \n -> testBit (bit n :: BitField) n
        

