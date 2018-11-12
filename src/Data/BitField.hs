module Data.BitField where

import Data.Word
import Data.Bits
import Data.Bool

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS

-- XXX: BitField should track its length as the entire byte might not be valid.

newtype BitField = BitField { unwrap :: BS.ByteString }
    deriving (Eq)

instance Show BitField where
    show (BitField as) = "BitField " ++ showbytes (BS.unpack as)
        where
            showbytes :: [Word8] -> String
            showbytes []       = ""
            showbytes (b:byte) = showbytes byte ++ showbits 7 b

            showbits :: Int -> Word8 -> String
            showbits n byte
                | n < 0     = ""
                | otherwise = bool '0' '1' (testBit byte n) : showbits (n-1) byte
    

bitwise :: (Word8 -> Word8 -> Word8) -> BS.ByteString -> BS.ByteString -> BS.ByteString
bitwise fn as bs
    | alen >= blen =
        (BS.pack $ BS.zipWith fn as bs) `BS.append` BS.drop blen as
    | otherwise =
        (BS.pack $ BS.zipWith fn as bs) `BS.append` BS.drop alen bs
    where
        alen = BS.length as
        blen = BS.length bs

instance Bits BitField where
    (BitField as) .&. (BitField bs) = BitField $ bitwise (.&.) as bs
    (BitField as) .|. (BitField bs) = BitField $ bitwise (.|.) as bs
    xor (BitField as) (BitField bs) = BitField $ bitwise xor as bs
    complement (BitField as) = BitField (BS.map complement as)

    shift (BitField as) = error "TODO"

    rotate (BitField as) n = BitField (BS.map (flip rotate n) as)
    bit n = BitField . BS.pack $ go n
        where
            go n
                | n >= 8 = 0 : go (n-8)
                | otherwise = [bit n :: Word8]

    testBit (BitField as) n
        | n >= 8     = testBit (BitField $ BS.drop 1 as) (n-8)
        | BS.null as = False
        | otherwise  = testBit (BS.head as) n

    bitSize                = error "TODO"
    bitSizeMaybe _         = Nothing
    isSigned _             = False
    popCount (BitField as) = sum (map popCount $ BS.unpack as)

length :: BitField -> Int
length (BitField bs) = BS.length bs * 8

bits :: BitField -> [Int]
bits bf = filter (testBit bf) [0..Data.BitField.length bf]
