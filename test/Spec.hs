import Data.Text (Text, intercalate, pack)
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
    Int,
    String,
    fst,
    map,
    mconcat,
    null,
    repeat,
    return,
    snd,
    take,
    ($),
    (+),
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
    (unformatted, formatted) <- generateExpression 4
    let preamble =
          "module X exposing (x)\n\
          \\n\
          \\n\
          \x =\n\
          \    "
    return (preamble <> unformatted <> "\n", preamble <> formatted <> "\n")

generateExpression :: Int -> Hedgehog.Gen (Text, Text)
generateExpression indent =
  Hedgehog.Gen.choice
    [ do
        text <-
          Hedgehog.Gen.text
            (Hedgehog.Range.constant 1 10)
            Hedgehog.Gen.digit
        return (text, text),
      generateList indent
    ]

generateList :: Int -> Hedgehog.Gen (Text, Text)
generateList indent =
  do
    items <- Hedgehog.Gen.list (Hedgehog.Range.constant 0 4) (generateExpression (indent + 2))
    Hedgehog.Gen.choice [generateNewlineList indent items, generateSingleLineList items]

generateNewlineList :: Int -> [(Text, Text)] -> Hedgehog.Gen (Text, Text)
generateNewlineList indent items =
  do
    spaces <- Hedgehog.Gen.choice $ map return ["\n ", "\n\n", "  \n ", "\n    \n "]
    return
      ( unformattedList spaces (map fst items),
        formattedMultiLineList indent (map snd items)
      )

generateSingleLineList :: [(Text, Text)] -> Hedgehog.Gen (Text, Text)
generateSingleLineList items =
  do
    spaces <- genSpaces
    return
      ( unformattedList spaces (map fst items),
        formattedSingleLineList (map snd items)
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

formattedSingleLineList :: [Text] -> Text
formattedSingleLineList items =
  if null items
    then "[]"
    else
      mconcat
        [ "[ ",
          intercalate ", " items,
          " ]"
        ]

formattedMultiLineList :: Int -> [Text] -> Text
formattedMultiLineList indent items =
  if null items
    then "[]"
    else
      let spaces = "\n" <> (pack $ take indent $ repeat ' ')
       in mconcat
            [ "[ ",
              intercalate ("," <> spaces) items,
              "\n" <> spaces <> "]"
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
