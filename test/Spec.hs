import Data.Text (Text)
import qualified Hedgehog
import qualified Hedgehog.Gen
import qualified Hedgehog.Range
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import qualified Test.Tasty.Hedgehog
import qualified Trumat
import Prelude (Either (..), IO, String, map, return, ($), (<>))

main :: IO ()
main =
  defaultMain $
    testGroup "Unit tests" $
      map oneTest cases <> [property]

property :: TestTree
property =
  Test.Tasty.Hedgehog.testProperty
    "format expression property"
    formatExpressionProperty

formatExpressionProperty :: Hedgehog.Property
formatExpressionProperty =
  Hedgehog.property $
    do
      (unformatted, formatted) <- Hedgehog.forAll generateModule
      Trumat.trumat unformatted Hedgehog.=== Right formatted

generateModule :: Hedgehog.Gen (Text, Text)
generateModule =
  do
    (unformatted, formatted) <- generateExpression
    let preamble =
          "module X exposing (x)\n\
          \\n\
          \\n\
          \x =\n\
          \    "
    return (preamble <> unformatted <> "\n", preamble <> formatted <> "\n")

generateExpression :: Hedgehog.Gen (Text, Text)
generateExpression =
  do
    text <-
      Hedgehog.Gen.text
        (Hedgehog.Range.constant 1 10)
        Hedgehog.Gen.digit
    return (text, text)

oneTest :: (String, Text, Text) -> TestTree
oneTest (name, input, expected) =
  testCase name $
    Trumat.trumat input Test.Tasty.HUnit.@?= Right expected

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
