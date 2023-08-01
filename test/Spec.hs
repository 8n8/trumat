import Data.Text (Text)
import Hedgehog (Property, forAll, property, (===))
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import Test.Tasty.Hedgehog (testProperty)
import qualified Trumat

main :: IO ()
main =
  defaultMain $
    testGroup "Unit tests" $
      properties : map oneTest cases

oneTest :: (String, Text, Text) -> TestTree
oneTest (name, input, expected) =
  testCase name $
    Trumat.trumat input Test.Tasty.HUnit.@?= Right expected

properties :: TestTree
properties =
  testProperty "trivial fuzz test" prop_reverse

prop_reverse :: Property
prop_reverse =
  property $ do
    xs <- forAll $ Gen.list (Range.linear 0 100) Gen.alpha
    reverse (reverse xs) === xs

cases :: [(String, Text, Text)]
cases =
  [ ( "hello world formatted",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \"
    )
  ]
