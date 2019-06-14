import Test.Tasty (defaultMain, testGroup)
import Test.Tasty.HUnit (assertEqual, testCase)
import MsgPack (pack, unpack)

main = defaultMain unitTests

unitTests =
  testGroup
    "Unit tests"
      [testReverseBoolTrue, testReverseBoolFalse, testReverseNil]

testReverseBoolTrue =
  testCase "Deserialising a serialised True (bool) gives a Just True" $ assertEqual [] (unpack $ pack True) (Just True)

testReverseBoolFalse =
  testCase "Deserialising a serialised False (bool) gives a Just False" $ assertEqual [] (unpack $ pack False) (Just False)

testReverseNil =
  testCase "Deserialising a serialised Nil gives a Just Nil" $ assertEqual [] (unpack $ pack ()) (Just ())
