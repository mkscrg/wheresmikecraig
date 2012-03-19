module Data.Bson.Aeson where

import qualified Data.Aeson as A
import Data.Attoparsec.Number (Number (..))
import qualified Data.Bson as B
import Data.Bson (Field (..))
import Data.CompactString.Internal (CompactString (..))
import qualified Data.HashMap.Lazy as H
import qualified Data.Vector as V
import Data.Text (Text, pack)
import Data.Text.Encoding (decodeUtf8, encodeUtf8)


textToUString :: Text -> B.UString
textToUString = CS . encodeUtf8

uStringToText :: B.UString -> Text
uStringToText = decodeUtf8 . unCS

aToB :: A.Value -> B.Value
aToB (A.Object v) = B.Doc $ map go $ H.toList v
  where go (k, v') = textToUString k := aToB v'
aToB (A.Array v) = B.Array $ map aToB $ V.toList v
aToB (A.String v) = B.String $ textToUString v
aToB (A.Number (I v)) = B.val v
aToB (A.Number (D v)) = B.val v
aToB (A.Bool v) = B.Bool v
aToB A.Null = B.Null

bToA :: B.Value -> A.Value
bToA (B.Float v) = A.Number $ D v
bToA (B.String v) = A.String $ uStringToText v
bToA (B.Doc v) = A.Object $ H.fromList $ map go v
  where go (k := v') = (uStringToText k, bToA v')
bToA (B.Array v) = A.Array $ V.fromList $ map bToA v
bToA (B.Bool v) = A.Bool v
bToA B.Null = A.Null
bToA (B.Int32 v) = A.Number $ I $ fromIntegral v
bToA (B.Int64 v) = A.Number $ I $ fromIntegral v
bToA v = A.String $ pack $ show v

-- Orphan instance! Uh oh!
instance A.FromJSON B.Value where
  parseJSON = return . aToB

-- Orphan instance! Uh oh!
instance A.ToJSON B.Value where
  toJSON = bToA
