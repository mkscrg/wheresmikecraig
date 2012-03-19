module Data.Bson.Aeson where

import qualified Data.Aeson as A
import Data.Attoparsec.Number (Number (..))
import qualified Data.Bson as B
import Data.Bson (Field (..))
import Data.CompactString.Internal (CompactString (CS))
import qualified Data.HashMap.Lazy as H
import qualified Data.Vector as V
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)


textToUString :: Text -> B.UString
textToUString = CS . encodeUtf8

aToB :: A.Value -> B.Value
aToB (A.Object v) = B.Doc $ map go $ H.toList v
  where go (k, v') = textToUString k := aToB v'
aToB (A.Array v) = B.Array $ map aToB $ V.toList v
aToB (A.String v) = B.String $ textToUString v
aToB (A.Number (I i)) = B.val i
aToB (A.Number (D d)) = B.val d
aToB (A.Bool b) = B.Bool b
aToB A.Null = B.Null

-- Orphan instance! Uh oh!
instance A.FromJSON B.Value where
  parseJSON = return . aToB
