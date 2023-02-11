import Data.Text (Text, intercalate)
import qualified Hedgehog
import qualified Hedgehog.Gen
import qualified Hedgehog.Range
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import qualified Test.Tasty.Hedgehog
import qualified Trumat
import Prelude
  ( Either (..),
    IO,
    String,
    fst,
    map,
    mconcat,
    null,
    return,
    snd,
    ($),
    (<>),
  )

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
  Hedgehog.Gen.choice
    [ do
        text <-
          Hedgehog.Gen.text
            (Hedgehog.Range.constant 1 10)
            Hedgehog.Gen.digit
        return (text, text),
      generateList
    ]

generateList :: Hedgehog.Gen (Text, Text)
generateList =
  do
    items <- Hedgehog.Gen.list (Hedgehog.Range.constant 0 4) generateExpression
    spaces <- genSpaces
    return
      ( unformattedList spaces (map fst items),
        formattedList (map snd items)
      )

genSpaces :: Hedgehog.Gen Text
genSpaces =
  Hedgehog.Gen.choice $ map return ["", " ", "  ", "   ", "    "]

unformattedList :: Text -> [Text] -> Text
unformattedList spaces items =
  if null items
    then
      mconcat
        [ "[",
          spaces,
          "]"
        ]
    else
      mconcat
        [ "[",
          spaces,
          intercalate (spaces <> "," <> spaces) items,
          spaces,
          "]"
        ]

formattedList :: [Text] -> Text
formattedList items =
  if null items
    then "[]"
    else
      mconcat
        [ "[ ",
          intercalate ", " items,
          " ]"
        ]

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
