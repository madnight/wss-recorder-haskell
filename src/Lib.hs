module Lib
    ( decodeBSON
    ) where

import Data.HashMap.Strict as Map (fromList, toList)
import Data.Bson as BSON
import Data.Aeson.Types as AESON
import Data.ByteString.Lazy.Internal
import Data.Vector as Vector (toList)
import Data.Aeson (decode, toJSON, Object)
import Data.Maybe (fromJust)
import Data.Scientific

bsonifyValue :: AESON.Value -> BSON.Value
bsonifyValue (Object obj) = Doc $ toBson obj
bsonifyValue (AESON.Array array) = BSON.Array . map bsonifyValue . Vector.toList $ array
bsonifyValue (AESON.String str) = BSON.String str
bsonifyValue (Number n) = case floatingOrInteger n of
                            Left float -> Float float
                            Right int -> Int64 $ fromIntegral int
bsonifyValue (AESON.Bool b) = BSON.Bool b
bsonifyValue _ = BSON.Null

decode' :: ByteString -> Object
decode' x = fromJust (decode x :: Maybe Object)

toBson :: Object -> Document
toBson = map (\(t, v) -> (t := bsonifyValue v)) . Map.toList

decodeBSON = toBson . decode'
