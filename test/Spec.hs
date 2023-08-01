import Data.Text (Text, pack)
import qualified Data.Text as Text
import Elm (Elm)
import qualified Elm
import Hedgehog (Gen, Property, forAll, property, (===))
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
      [properties, units]

units :: TestTree
units =
  testGroup "Unit tests" (map oneTest cases)

oneTest :: (String, Text, Text) -> TestTree
oneTest (name, input, expected) =
  testCase name $
    Trumat.trumat input Test.Tasty.HUnit.@?= Right expected

properties :: TestTree
properties =
  testGroup
    "Property tests"
    [ testProperty "trivial fuzz test" prop_reverse,
      testProperty "same code, same formatted" sameCodeSameFormatted
    ]

sameCodeSameFormatted :: Property
sameCodeSameFormatted =
  property $ do
    ast <- forAll genAst
    unformatted1 <- forAll $ genUnformatted ast
    unformatted2 <- forAll $ genUnformatted ast
    Trumat.trumat unformatted1 === Trumat.trumat unformatted2

genUnformatted :: Elm -> Gen Text
genUnformatted (Elm.Expression int) =
  do
    leadingSpaces <- genSomeSpaces
    return $
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\\n\
      \"
        <> leadingSpaces
        <> pack (show int)
        <> "\n"

genAst :: Gen Elm
genAst =
  do
    int <- Gen.int (Range.linear 0 10000)
    return $ Elm.Expression int

genSomeSpaces :: Gen Text
genSomeSpaces =
  do
    n <- Gen.int (Range.linear 1 1000)
    return $ Text.replicate n " "

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
