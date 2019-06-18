module MsgPack (FromMsgPack, ToMsgPack, pack, unpack) where

import qualified Data.ByteString.Lazy as BS
import Data.Binary.Put (putWord8, Put, runPut, putInt32be)
import Data.Binary.Get (Get, getWord8, runGet, getInt32be, getInt16be)
import Data.Int (Int32)
import Data.Word (Word32)
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

instance ToMsgPack Int32 where
  pack b = runPut $ unparseObject b
  unparseObject i = do
    putWord8 0xd2
    putInt32be i

instance FromMsgPack Int32 where
  parseObject = do
    msgPackType <- getWord8
    val <- getInt32be
    return $ case msgPackType of
      0xd2 -> Just val
      _ -> Nothing

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
