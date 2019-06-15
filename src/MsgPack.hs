{-# LANGUAGE FlexibleInstances #-}

module MsgPack (FromMsgPack, ToMsgPack, pack, unpack) where

import qualified Data.ByteString.Lazy as BS
import Data.Binary.Put (putWord8, Put, runPut, putInt32be)
import Data.Binary.Get (Get, getWord8, runGet, getInt32be, getInt16be)
import Data.Int (Int32)
import Data.Word (Word32)
import Control.Monad (fmap, forever, forM)

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

parseNil :: Get (Maybe ())
parseNil = do
  msgpackType <- getWord8
  return (case msgpackType of
    0xc0 -> Just ()
    _    -> Nothing)

instance FromMsgPack () where
  unpack b = runGet parseNil b

unparseNil :: () -> Put
unparseNil () = putWord8 0xc0

instance ToMsgPack () where
  pack b = runPut $ unparseNil b

unparseInt32 :: Int32 -> Put
unparseInt32 i = do
    putWord8 0xd2
    putInt32be i

instance ToMsgPack Int32 where
  pack b = runPut $ unparseInt32 b

parseInt32 :: Get (Maybe Int32)
parseInt32 = do
    msgPackType <- getWord8
    val <- getInt32be
    return $ case msgPackType of
      0xd2 -> Just val
      _ -> Nothing

instance FromMsgPack Int32 where
  unpack b = runGet parseInt32 b

unparseArray32Header :: [a] -> Put
unparseArray32Header i = do
    putWord8 0xdd
    putInt32be (fromIntegral (length i) :: Int32)

parseArrayHeader :: Get (Maybe Int32)
parseArrayHeader = do
    msgPackType <- getWord8
    --size <- getInt32be
    case msgPackType of
        0xdd -> fmap Just getInt32be
        0xdc -> fmap (Just . (\x -> (fromIntegral x :: Int32))) getInt16be
        _ -> return $ Nothing

packVec :: ToMsgPack a => [a] -> BS.ByteString
packVec b = do
  let header = runPut $ unparseArray32Header b
  let serialisedArray = fmap pack b
  BS.concat $ [header] ++ serialisedArray

unpackVec :: FromMsgPack a => BS.ByteString -> Maybe [a]
unpackVec b = do
    let numElems = runGet parseArrayHeader b
    case numElems of
      Just 0 -> Just []
      Just n -> forM [1..1] (\y -> unpack b)
      _ -> Nothing

