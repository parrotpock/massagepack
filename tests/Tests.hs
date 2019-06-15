import Test.Tasty (defaultMain, testGroup)
import Test.Tasty.HUnit (assertEqual, testCase)
import MsgPack (pack, unpack)
import Data.Int (Int32)

main = defaultMain unitTests

unitTests =
  testGroup
    "Unit tests"
      [testReverseBoolTrue, testReverseBoolFalse, testReverseNil, testReverseInt32]

testReverseBoolTrue =
  testCase "Deserialising a serialised True (bool) gives a Just True" $ assertEqual [] (unpack $ pack True) (Just True)

testReverseBoolFalse =
  testCase "Deserialising a serialised False (bool) gives a Just False" $ assertEqual [] (unpack $ pack False) (Just False)

testReverseNil =
  testCase "Deserialising a serialised Nil gives a Just Nil" $ assertEqual [] (unpack $ pack ()) (Just ())

testReverseInt32 =
  let val = 234234::Int32 in
  testCase "Deserialising a serialised Int32 gives a Just Int32" $ assertEqual [] (unpack $ pack val) (Just val)
