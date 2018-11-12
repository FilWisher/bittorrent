module Data.BitTorrent.Message.Test where

import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Arbitrary

import Control.Monad (replicateM)

import           Data.BitTorrent.Message (Message(..), Handshake(..))
import qualified Data.BitTorrent.Message as Message
import qualified Data.BitField as BF
import qualified Data.ByteString as BS
import qualified Data.Serialize as S
import           Data.Word

instance Arbitrary Handshake where
    arbitrary = Handshake <$> bytestring <*> bytestring
        where
            bytestring = BS.pack <$> (replicateM 20 arbitrary)

instance Arbitrary Message where
    arbitrary = oneof
        [ pure KeepAlive
        , pure Choke
        , pure Unchoke
        , pure Interested
        , pure NotInterested
        , Have <$> arbitrary
        , BitField . BF.BitField . BS.pack <$> arbitrary
        , Request <$> arbitrary <*> arbitrary <*> arbitrary
        , Piece <$> arbitrary <*> arbitrary <*> (BS.pack <$> arbitrary)
        , Cancel<$> arbitrary <*> arbitrary <*> arbitrary
        , Port . fromIntegral <$> (arbitrary :: Gen Word16)
        ]

tests :: Spec
tests =
    describe "Data.BitTorrent.Message" $
        it "decode . encode == id" $ property $ \m ->
            either (const False) (== m) (S.decode $ S.encode (m :: Message))
            
