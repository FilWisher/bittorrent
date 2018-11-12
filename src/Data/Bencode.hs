{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}
{-# LANGUAGE FlexibleInstances #-}

module Data.Bencode where

import Debug.Trace

import Control.Applicative ((<|>))

import           Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.ByteString.Char8 as ASCII
import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import           Data.Serialize (Serialize(..))
import qualified Data.Serialize as S
import           Data.Attoparsec.ByteString (Parser(..))
import qualified Data.Attoparsec.ByteString as A
import           Data.Hashable (Hashable)
import           Data.Char (chr, isDigit)
import           Data.Word (Word8)


data Value
    = String BS.ByteString
    | Number Int
    | List [Value]
--    -- XXX: use a hashmap like aeson?
    | Dictionary (Map Value Value)
    deriving (Eq, Ord)

instance Show Value where
    show v = showInd 0 v
        where
            showInd :: Int -> Value -> String
            showInd n (Number m) = (concat (replicate n "  ")) ++ show m
            showInd n (String bs) = (concat (replicate n "  ")) ++ show bs
            showInd n v =
                case v of
                    List v -> concat (replicate n "  ") ++ "[\n" ++ concatMap (\v -> showInd (n+1) v ++ ",\n") v ++ "\n]"
                    Dictionary d -> concat (replicate n "  ") ++ "{\n" 
                        ++ concatMap (\(k,v) -> showInd (n+1) k ++ ": " ++ showInd 0 v ++ ",\n") (Map.toList d)
                        ++ "\n}"

asciiNum :: Int -> BS.ByteString
asciiNum = ASCII.pack . show

wrap :: Char -> S.Put -> Char -> S.Put
wrap start contents end = do
    S.putByteString (ASCII.singleton start)
    contents
    S.putByteString (ASCII.singleton end)


encode :: Value -> BS.ByteString
encode val = S.runPut $ 
    case val of
        (String str) -> do
            S.putByteString (asciiNum $ BS.length str)
            S.putByteString ":"
            S.putByteString str
        (Number int)      -> wrap 'i' (S.putByteString $ asciiNum int) 'e'
        (List ls)         -> wrap 'l' (mapM_ (S.putByteString . encode) ls) 'e'
        (Dictionary dict) -> wrap 'd' (mapM_ keypairs $ Map.toList dict) 'e'
    where
        keypairs (k,v) = S.putByteString (encode k) >> S.putByteString (encode v)

decode :: BS.ByteString -> Either String Value
decode = A.parseOnly parseValue

parseValue :: A.Parser Value
parseValue = A.choice
    [ parseNumber
    , parseString
    , parseList
    , parseDictionary
    ]

parseList :: A.Parser Value
parseList = do
    A.satisfy (==108)
    List <$> A.manyTill' parseValue (A.satisfy (==101))
    
parseString :: A.Parser Value
parseString = do
    len <- read . tostring <$> A.many1 (A.satisfy digit)
    A.satisfy (==58)
    String <$> A.take len

tostring :: [Word8] -> String
tostring = ASCII.unpack . BS.pack
    
digit :: Word8 -> Bool
digit c = c >= 48 && c <= 57

number :: A.Parser BS.ByteString
number =
    (A.satisfy (==45) >>= \c -> (c `BS.cons`) <$> nat) <|> nat
    where
        nat = A.takeWhile digit

parseNumber :: A.Parser Value
parseNumber = do
     A.satisfy (==105)
     num <- number
     A.satisfy (==101)
     return . Number . read . ASCII.unpack $ num

parseDictionary :: A.Parser Value
parseDictionary = do
    A.satisfy (==100)
    Dictionary . Map.fromList <$> A.manyTill' parsePair (A.satisfy (==101))
    where
        parsePair = (,) <$> parseString <*> parseValue

(.:) :: FromBencode a => Value -> BS.ByteString -> Either String a
(.:) (Dictionary d) str = 
    maybe (Left $ "No such key: " ++ show str) fromBencode (Map.lookup (String str) d)
(.:) _ str = Left ("Value not a dictionary: " ++ show str)

(.:?) :: FromBencode a => Value -> BS.ByteString -> Either String (Maybe a)
(.:?) (Dictionary d) str =
    case Map.lookup (String str) d of
        Nothing -> return Nothing
        Just v  -> Just <$> fromBencode v
(.:?) _ str = Left ("Value not a dictionary: " ++ show str)

class FromBencode a where
    fromBencode :: Value -> Either String a

instance FromBencode String where
    fromBencode (String bs) = Right (show bs)
    fromBencode _           = Left "Not a string"

instance FromBencode BS.ByteString where
    fromBencode (String bs) = Right bs
    fromBencode _           = Left "Not a string"

instance FromBencode Int where
    fromBencode (Number n) = Right (fromIntegral n)
    fromBencode _          = Left "Not a Number"

instance FromBencode Integer where
    fromBencode (Number n) = Right (fromIntegral n)
    fromBencode _          = Left "Not a Number"

instance FromBencode a => FromBencode [a] where
    fromBencode (List as) = traverse fromBencode as
    fromBencode _         = Left "Not a List"

instance (Ord a, FromBencode a, FromBencode b) => FromBencode (Map a b) where
    fromBencode (Dictionary d) =
        Map.fromList <$> traverse (\(k, v) -> (,) <$> fromBencode k <*> fromBencode v) (Map.toList d)
    fromBencode _ = Left "Not a Dictionary"

instance FromBencode Value where
    fromBencode = Right . id

unbencode :: FromBencode a => BS.ByteString -> Either String a
unbencode = (fromBencode =<<) . decode

--class ToBencode a where
--    toBencode :: a -> Value
--
--class FromBencode a where
--    fromBencode :: Value -> Either String a
--
--instance ToBencode a => ToBencode [a] where
--    toBencode ls = List $ map toBencode ls
