module Props (prop_reversable) where

import MsgPack

--prop_reversable :: (ToMsgPack a, FromMsgPack a, Eq a) => a -> Bool
prop_reversable :: (ToMsgPack a, FromMsgPack a, Eq a) => a -> Bool
prop_reversable t = do
  x == t
    where
  Just x = (unpack $ pack t)
