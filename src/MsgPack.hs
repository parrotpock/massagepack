
module MsgPack (FromMsgPack, ToMsgPack, pack, unpack) where

import qualified Data.ByteString.Lazy as BS
import Data.Binary.Put (putWord8, Put, runPut)
import Data.Binary.Get (Get, getWord8, runGet)

class FromMsgPack a where
  unpack :: BS.ByteString -> Maybe a

class ToMsgPack a where
  pack :: a -> BS.ByteString

unparseBool :: Bool -> Put
unparseBool False = putWord8 0xc2
unparseBool True = putWord8 0xc3

instance ToMsgPack Bool where
  pack b = runPut $ unparseBool b

parseBool :: Get (Maybe Bool)
parseBool = do
  msgpackType <- getWord8
  return (case msgpackType of
    0xc2 -> Just False
    0xc3 -> Just True
    _    -> Nothing)

instance FromMsgPack Bool where
  unpack b = runGet parseBool b

