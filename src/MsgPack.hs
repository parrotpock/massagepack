module MsgPack (FromMsgPack, ToMsgPack, pack, unpack) where

import qualified Data.ByteString.Lazy as BS
import qualified Data.ByteString.Char8 as SBS
import Data.Binary.Put (putWord8, Put, runPut, putInt32be, putInt8, putInt16be, putInt64be, putWord16be, putWord32be, putWord64be, putFloatbe, putDoublebe)
import Data.Binary.Get (Get, getWord8, getWord16be, getWord32be, getWord64be, runGet, getInt32be, getInt16be, getInt64be, getInt8, getFloatbe, getDoublebe, getByteString)
import Data.Int (Int32, Int8, Int16, Int64)
import Data.Word (Word32, Word8, Word16, Word64)
import Control.Monad (replicateM)
import Data.Maybe
import System.CPUTime
import Text.Printf


class FromMsgPack a where
  unpack :: BS.ByteString -> Maybe a
  parseObject :: Get (Maybe a)

class ToMsgPack a where
  pack :: a -> BS.ByteString
  unparseObject :: a -> Put

-- Bool
instance ToMsgPack Bool where
  pack b = runPut $ unparseObject b
  unparseObject False = putWord8 0xc2
  unparseObject True = putWord8 0xc3

instance FromMsgPack Bool where
  parseObject = do
    msgpackType <- getWord8
    return (case msgpackType of
      0xc2 -> Just False
      0xc3 -> Just True
      _    -> Nothing)
  unpack b = runGet parseObject b

-- Void
instance FromMsgPack () where
  parseObject = do
    msgpackType <- getWord8
    return (case msgpackType of
      0xc0 -> Just ()
      _    -> Nothing)

  unpack b = runGet parseObject b

instance ToMsgPack () where
  pack b = runPut $ unparseObject b
  unparseObject () = putWord8 0xc0

-- Numbers
parseNumerical :: (Get a) -> (Word8 -> Bool) -> Get (Maybe a)
parseNumerical typeFunc mpTypeByteTest = do
  msgPackType <- getWord8
  val <- typeFunc
  return (if (mpTypeByteTest msgPackType)
            then (Just val)
            else Nothing)

unparseNumerical :: (a -> Put) -> Word8 -> a -> Put
unparseNumerical putFunc mpTypeByte val = do
  putWord8 mpTypeByte
  putFunc val

-- Int8
instance FromMsgPack Int8 where
  parseObject = do
    msgPackType <- getInt8
    case msgPackType of
      y | y >= 0x00 && y <= 0x7f -> return $ Just y
      y | y == 0xcc -> getInt8 >>= (return . Just)
      _ -> return $ Nothing
  unpack b = runGet parseObject b

instance ToMsgPack Int8 where
  pack b = runPut $ unparseObject b
  unparseObject x = case x of
    y | y >= 0x00 && y <= 0x7f -> putInt8 x
    y | y >= 0x7f -> putWord8 0xcc >> putInt8 x

-- Int16
instance ToMsgPack Int16 where
  pack b = runPut $ unparseObject b
  unparseObject x = unparseNumerical putInt16be 0xd1 x

instance FromMsgPack Int16 where
  parseObject = parseNumerical getInt16be (\x -> x == 0xd1)
  unpack b = runGet parseObject b

-- Int32
instance ToMsgPack Int32 where
  pack b = runPut $ unparseObject b
  unparseObject i = unparseNumerical putInt32be 0xd2 i

instance FromMsgPack Int32 where
  parseObject = parseNumerical getInt32be (\x -> x == 0xd2)
  unpack b = runGet parseObject b

-- Int64
instance ToMsgPack Int64 where
  pack b = runPut $ unparseObject b
  unparseObject i = unparseNumerical putInt64be 0xd3 i

instance FromMsgPack Int64 where
  parseObject = parseNumerical getInt64be (\x -> x == 0xd3)
  unpack b = runGet parseObject b

-- Word8/UInt8
instance FromMsgPack Word8 where
  parseObject = do
    msgpackType <- getWord8
    return (case msgpackType of
      x | x >= 0x00 && x <= 0x7f -> Just x
      _ -> Nothing)

  unpack b = runGet parseObject b

instance ToMsgPack Word8 where
  pack b = runPut $ unparseObject b
  unparseObject x = case x of
    y | y >= 0x00 && y <= 0x7f -> putWord8 x
    y | y >= 0x7f -> putWord8 0xcc >> putWord8 x

-- Word16/UInt16
instance ToMsgPack Word16 where
  pack b = runPut $ unparseObject b
  unparseObject i = unparseNumerical putWord16be 0xd1 i

instance FromMsgPack Word16 where
  parseObject = parseNumerical getWord16be (\x -> x == 0xd1)
  unpack b = runGet parseObject b

-- Word32/Int32
instance ToMsgPack Word32 where
  pack b = runPut $ unparseObject b
  unparseObject i = unparseNumerical putWord32be 0xd2 i

instance FromMsgPack Word32 where
  parseObject = parseNumerical getWord32be (\x -> x == 0xd2)
  unpack b = runGet parseObject b

-- Word64/Int64
instance ToMsgPack Word64 where
  pack b = runPut $ unparseObject b
  unparseObject i = unparseNumerical putWord64be 0xd3 i

instance FromMsgPack Word64 where
  parseObject = parseNumerical getWord64be (\x -> x == 0xd3)
  unpack b = runGet parseObject b

-- Float 32
instance ToMsgPack Float where
  pack b = runPut $ unparseObject b
  unparseObject i = unparseNumerical putFloatbe 0xca i

instance FromMsgPack Float where
  parseObject = parseNumerical getFloatbe (\x -> x == 0xca)
  unpack b = runGet parseObject b

-- Float 64
instance ToMsgPack Double where
  pack b = runPut $ unparseObject b
  unparseObject i = unparseNumerical putDoublebe 0xcb i

instance FromMsgPack Double where
  parseObject = parseNumerical getDoublebe (\x -> x == 0xcb)
  unpack b = runGet parseObject b

-- ByteString
instance FromMsgPack SBS.ByteString where
  parseObject = do
    msgPackType <- getInt8
    -- If the type is below 160 then we have a fixed string
    if (msgPackType >= 0xa0) && (msgPackType <= 0xbf) then
      sequence $ Just $ getByteString (((fromIntegral $ msgPackType) :: Int) - 160)
    else
      return $ Nothing

  unpack b = runGet parseObject b

unparseArray32Header :: [a] -> Put
unparseArray32Header i = do
    putWord8 0xdd
    putInt32be (fromIntegral (length i) :: Int32)

parseArrayHeader :: Get (Maybe Int32)
parseArrayHeader = do
    msgPackType <- getWord8
    case msgPackType of
        0xdd -> fmap Just getInt32be
        0xdc -> fmap (Just . (\x -> (fromIntegral x :: Int32))) getInt16be
        _ -> return $ Nothing

instance ToMsgPack a => ToMsgPack [a] where
  pack b = do
    runPut $ unparseObject b
  unparseObject os = do
    unparseArray32Header os
    unparseList os
      where
    unparseList [] = return ()
    unparseList (o:os) = do
      unparseObject o
      unparseList os

instance FromMsgPack a => FromMsgPack [a] where
  unpack b = runGet parseObject b
  parseObject = do
    numElems <- parseArrayHeader
    case numElems of
      Just 0 -> return $ Just []
      Just n -> do
        val <- replicateM (fromIntegral n :: Int) parseObject
        return $ sequence val
      _ -> return $ Nothing
