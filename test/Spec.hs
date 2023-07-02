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
    fmap,
    map,
    mapM,
    mconcat,
    repeat,
    return,
    take,
    ($),
    (+),
    (<>),
  )

main :: IO ()
main =
  defaultMain $
    testGroup "Unit tests" $ map oneTest cases

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
    listType <- Hedgehog.Gen.element [SingleLineList, MultiLineList]
    (unformatted, formatted) <- generateExpression 4 listType
    let preamble =
          "module X exposing (x)\n\
          \\n\
          \\n\
          \x =\n\
          \    "
    return (preamble <> unformatted <> "\n", preamble <> formatted <> "\n")

generateExpression ::
  Int ->
  ([Expression] -> Expression) ->
  Hedgehog.Gen (Text, Text)
generateExpression indent listType =
  do
    ast <- generateAst listType
    unformatted <- printUnformatted indent ast
    return (unformatted, printFormatted indent ast)

generateAst :: ([Expression] -> Expression) -> Hedgehog.Gen Expression
generateAst listType =
  Hedgehog.Gen.choice
    [ fmap Verbatim generateVerbatimAst,
      generateListAst listType
    ]

generateVerbatimAst :: Hedgehog.Gen Text
generateVerbatimAst =
  Hedgehog.Gen.text
    (Hedgehog.Range.constant 1 10)
    Hedgehog.Gen.digit

generateListAst :: ([Expression] -> Expression) -> Hedgehog.Gen Expression
generateListAst listType =
  -- Having the upper limit higher than 3 causes the tests to hang. I
  -- guess it has to generate too much stuff.
  fmap listType $
    Hedgehog.Gen.list (Hedgehog.Range.constant 0 3) (generateAst listType)

generateNewlineSpaces :: Hedgehog.Gen Text
generateNewlineSpaces =
  Hedgehog.Gen.element ["\n ", "\n\n ", "    \n ", "\n\n \n ", " \n \n "]

printUnformatted :: Int -> Expression -> Hedgehog.Gen Text
printUnformatted indent expression =
  case expression of
    Verbatim text ->
      return text
    SingleLineList [] ->
      do
        spaces <- genSpaces
        return $ "[" <> spaces <> "]"
    SingleLineList oneOrMore ->
      do
        spaces <- genSpaces
        printed <- mapM (printUnformatted (indent + 2)) oneOrMore
        return $
          mconcat
            [ "[",
              spaces,
              intercalate ("," <> spaces) printed,
              spaces,
              "]"
            ]
    MultiLineList [] ->
      do
        spaces <- generateNewlineSpaces
        return $ "[" <> spaces <> "]"
    MultiLineList oneOrMore ->
      do
        spaces <- generateNewlineSpaces
        printed <- mapM (printUnformatted (indent + 2)) oneOrMore
        return $
          mconcat
            [ "[",
              intercalate ("," <> spaces) printed,
              spaces,
              "]"
            ]

printFormatted :: Int -> Expression -> Text
printFormatted indent expression =
  case expression of
    Verbatim text ->
      text
    SingleLineList [] ->
      "[]"
    SingleLineList oneOrMore ->
      mconcat
        [ "[ ",
          intercalate ", " (map (printFormatted (indent + 2)) oneOrMore),
          " ]"
        ]
    MultiLineList [] ->
      "[]"
    MultiLineList oneOrMore ->
      let spaces = "\n" <> pack (take indent (repeat ' '))
       in mconcat
            [ "[ ",
              intercalate
                (spaces <> ", ")
                (map (printFormatted (indent + 2)) oneOrMore),
              spaces <> "]"
            ]

data Expression
  = Verbatim Text
  | SingleLineList [Expression]
  | MultiLineList [Expression]

genSpaces :: Hedgehog.Gen Text
genSpaces =
  Hedgehog.Gen.choice $ map return ["", " ", "  ", "   ", "    "]

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
