import Control.Exception (try)
import qualified Data.ByteString
import Data.Text (Text, intercalate, pack)
import qualified Data.Text.Encoding
import qualified Hedgehog
import qualified Hedgehog.Gen
import qualified Hedgehog.Range
import qualified System.Directory
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import qualified Test.Tasty.Hedgehog
import qualified Trumat
import Prelude
  ( Bool (..),
    Either (..),
    FilePath,
    IO,
    IOError,
    Int,
    String,
    drop,
    fail,
    fmap,
    map,
    mapM,
    mconcat,
    print,
    pure,
    repeat,
    return,
    reverse,
    take,
    ($),
    (+),
    (<>),
  )

main :: IO ()
main =
  do
    paths <- fmap (map (\path -> drop 11 path)) (getElmPaths "test_input")
    defaultMain $
      testGroup "Unit tests" $
        regressionTests paths : property : map oneTest cases

regressionTests :: [FilePath] -> TestTree
regressionTests paths =
  testGroup "Regression tests" $
    map oneRegressionTest paths

oneRegressionTest :: FilePath -> TestTree
oneRegressionTest path =
  testCase path $
    do
      inputBytes <- Data.ByteString.readFile ("test_input/" <> path)
      expectedBytes <- Data.ByteString.readFile ("test_expected/" <> path)
      case ( Data.Text.Encoding.decodeUtf8' inputBytes,
             Data.Text.Encoding.decodeUtf8' expectedBytes
           ) of
        (Left _, _) ->
          fail "expecting UTF8 input file"
        (_, Left _) ->
          fail "expecting UTF8 expected output file"
        (Right input, Right expected) ->
          Trumat.trumat input Test.Tasty.HUnit.@?= Right expected

listDirectory :: FilePath -> IO (Either IOError [FilePath])
listDirectory path =
  try $ System.Directory.listDirectory path

isElmFile :: FilePath -> Bool
isElmFile path =
  case reverse path of
    'm' : 'l' : 'e' : '.' : _ ->
      True
    _ ->
      False

getElmPaths :: FilePath -> IO [FilePath]
getElmPaths path =
  do
    eitherContents <- listDirectory path
    case eitherContents of
      Left _ ->
        if isElmFile path
          then pure [path]
          else pure []
      Right contents ->
        fmap
          mconcat
          (mapM (\subPath -> getElmPaths (path <> "/" <> subPath)) contents)

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
    ),
    ( "nested lists",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    [ [ [ []\n\
      \        ]\n\
      \      ]\n\
      \    ]\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    [ [ [ []\n\
      \        ]\n\
      \      ]\n\
      \    ]\n\
      \"
    ),
    ( "multi-line list",
      "module X exposing (x)\n\n\nx =\n    [ 0\n    ]\n",
      "module X exposing (x)\n\n\nx =\n    [ 0\n    ]\n"
    ),
    ( "case of formatted",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    case x of\n\
      \        A ->\n\
      \            a\n\
      \\n\
      \        B ->\n\
      \            b\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    case x of\n\
      \        A ->\n\
      \            a\n\
      \\n\
      \        B ->\n\
      \            b\n\
      \"
    ),
    ( "case of few spaces",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    case x of\n\
      \ A->a\n\
      \ B->b\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    case x of\n\
      \        A ->\n\
      \            a\n\
      \\n\
      \        B ->\n\
      \            b\n\
      \"
    ),
    ( "case of multiline",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    case [x\n\
      \ ] of\n\
      \ A->a\n\
      \ B->b\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    case\n\
      \        [ x\n\
      \        ]\n\
      \    of\n\
      \        A ->\n\
      \            a\n\
      \\n\
      \        B ->\n\
      \            b\n\
      \"
    ),
    ( "if-then-else ordinary formatted",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    if a then\n\
      \        0\n\
      \    else\n\
      \        1\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    if a then\n\
      \        0\n\
      \\n\
      \    else\n\
      \        1\n\
      \"
    ),
    ( "if-then-else unformatted",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    if\n\
      \        a\n\
      \    then\n\
      \        0\n\
      \\n\
      \    else\n\
      \        1\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    if a then\n\
      \        0\n\
      \\n\
      \    else\n\
      \        1\n\
      \"
    ),
    ( "if-then-else single-line",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    if a then b else c\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    if a then\n\
      \        b\n\
      \\n\
      \    else\n\
      \        c\n\
      \"
    ),
    ( "if-then-else multi-line list",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    if [a\n\
      \ ] then b else c\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    if\n\
      \        [ a\n\
      \        ]\n\
      \    then\n\
      \        b\n\
      \\n\
      \    else\n\
      \        c\n\
      \"
    ),
    ( "tuple formatted",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    ( 1, 2 )\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    ( 1, 2 )\n\
      \"
    ),
    ( "tuple no spaces, single line",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    (1,2)\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    ( 1, 2 )\n\
      \"
    ),
    ( "multi-line tuple, formatted",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    ( 1\n\
      \    , 2\n\
      \    )\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    ( 1\n\
      \    , 2\n\
      \    )\n\
      \"
    ),
    ( "multi-line tuple, unformatted",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    (1,2\n\
      \ )\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    ( 1\n\
      \    , 2\n\
      \    )\n\
      \"
    ),
    ( "list inside tuple, formatted",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    ( [], 1 )\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    ( [], 1 )\n\
      \"
    ),
    ( "no singleton tuple",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    ( 1 )\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    1\n\
      \"
    ),
    ( "let in formatted",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    let\n\
      \        a =\n\
      \            1\n\
      \    in\n\
      \    a\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    let\n\
      \        a =\n\
      \            1\n\
      \    in\n\
      \    a\n\
      \"
    ),
    ( "let in multiple items, formatted",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    let\n\
      \        a =\n\
      \            0\n\
      \        b =\n\
      \            1\n\
      \    in\n\
      \    a\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    let\n\
      \        a =\n\
      \            0\n\
      \\n\
      \        b =\n\
      \            1\n\
      \    in\n\
      \    a\n\
      \"
    ),
    ( "let in, single-line",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    let a = 0 in a\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    let\n\
      \        a =\n\
      \            0\n\
      \    in\n\
      \    a\n\
      \"
    ),
    ( "single-line function call, formatted",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    a b\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    a b\n\
      \"
    ),
    ( "multi-line function call, formatted",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    a\n\
      \        b\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    a\n\
      \        b\n\
      \"
    ),
    ( "single-line function call, unformatted",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    a  b\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    a b\n\
      \"
    ),
    ( "multi-line function call, unformatted",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    a\n\
      \     b\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    a\n\
      \        b\n\
      \"
    ),
    ( "multi-line multi-argument function call, formatted",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    a\n\
      \        b\n\
      \        c\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    a\n\
      \        b\n\
      \        c\n\
      \"
    ),
    ( "multi-line multi-argument function call, unformatted",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    a\n\
      \                           b\n\
      \              c\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    a\n\
      \        b\n\
      \        c\n\
      \"
    ),
    ( "empty string literal",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    \"\"\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    \"\"\n\
      \"
    ),
    ( "simple string literal",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    \"hello\"\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    \"hello\"\n\
      \"
    ),
    ( "simple string literal with escaped quote",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    \"\\\"\"\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    \"\\\"\"\n\
      \"
    ),
    ( "simple string literal with escaped backslash",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    \"\\\\\"\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    \"\\\\\"\n\
      \"
    ),
    ( "empty triple-quote string literal",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    \"\"\"\"\"\"\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    \"\"\"\"\"\"\n\
      \"
    ),
    ( "single-line triple-quote string literal",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    \"\"\"a\"\"\"\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    \"\"\"a\"\"\"\n\
      \"
    ),
    ( "single-line triple-quote string literal containing quote mark",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    \"\"\" \" \"\"\"\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    \"\"\" \" \"\"\"\n\
      \"
    ),
    ( "single-line triple-quoted string literal containing escaped backslash",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    \"\"\"\\\\\"\"\"\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    \"\"\"\\\\\"\"\"\n\
      \"
    ),
    ( "multi-line triple-quoted string literal",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    \"\"\"\n\
      \\"\"\"\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    \"\"\"\n\
      \\"\"\"\n\
      \"
    ),
    ( "formatted empty record",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    {}\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    {}\n\
      \"
    ),
    ( "formatted record with one item",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    { a = 2 }\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    { a = 2 }\n\
      \"
    ),
    ( "multi-line tuple with only one item",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    ( a\n\
      \    )\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    a\n\
      \"
    ),
    ( "single-line multi-item formatted record",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    { a = 1, b = 2 }\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    { a = 1, b = 2 }\n\
      \"
    ),
    ( "single-line empty record with space in it",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    { }\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    {}\n\
      \"
    ),
    ( "multi-line empty record",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    {\n\
      \ }\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    {}\n\
      \"
    ),
    ( "multi-line formatted record with one single-line item",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    { a = 2\n\
      \    }\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    { a = 2\n\
      \    }\n\
      \"
    ),
    ( "multi-line unformatted record with one single-line item",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    {a=2\n\
      \ }\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    { a = 2\n\
      \    }\n\
      \"
    ),
    ( "multi-line formatted record with one multi-line item",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    { a =\n\
      \        0\n\
      \    }\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    { a =\n\
      \        0\n\
      \    }\n\
      \"
    ),
    ( "multi-line unformatted record with one multi-line item",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    { a= \n\
      \ 1}\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    { a =\n\
      \        1\n\
      \    }\n\
      \"
    ),
    ( "formatted multi-line record with two single-line items",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    { a = 1\n\
      \    , b = 2\n\
      \    }\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    { a = 1\n\
      \    , b = 2\n\
      \    }\n\
      \"
    ),
    ( "formatted single-line record update, with one item",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    { a | b = 0 }\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    { a | b = 0 }\n\
      \"
    ),
    ( "formatted single-line record update, with two items",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    { a | b = 0, c = 1 }\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    { a | b = 0, c = 1 }\n\
      \"
    ),
    ( "formatted multi-line record update, with one item",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    { a\n\
      \        | b = 0\n\
      \    }\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    { a\n\
      \        | b = 0\n\
      \    }\n\
      \"
    ),
    ( "formatted multi-line record update, with two items",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    { a\n\
      \        | b = 0\n\
      \        , c = 1\n\
      \    }\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    { a\n\
      \        | b = 0\n\
      \        , c = 1\n\
      \    }\n\
      \"
    ),
    ( "unformatted single-line record update, with one item",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    {a|b=2}\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    { a | b = 2 }\n\
      \"
    ),
    ( "unformatted multi-line record update, with one item",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    {a|b=0\n\
      \ }\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    { a\n\
      \        | b = 0\n\
      \    }\n\
      \"
    ),
    ( "line comment in a list, formatted",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    [-- a\n\
      \    ]\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    [-- a\n\
      \    ]\n\
      \"
    ),
    ( "line comment in a single-item list, formatted",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    [ -- a\n\
      \      0\n\
      \    ]\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    [ -- a\n\
      \      0\n\
      \    ]\n\
      \"
    ),
    ( "two leading line comments in a single-item list, formatted",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    [ -- a\n\
      \      -- b\n\
      \      0\n\
      \    ]\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    [ -- a\n\
      \      -- b\n\
      \      0\n\
      \    ]\n\
      \"
    ),
    ( "trailing line comment in a single-item list, formatted",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    [ 0\n\
      \\n\
      \    -- a\n\
      \    ]\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    [ 0\n\
      \\n\
      \    -- a\n\
      \    ]\n\
      \"
    ),
    ( "trailing line comment in multi-line list, formatted",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    [ 0\n\
      \\n\
      \    -- a\n\
      \    , 1\n\
      \    ]\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    [ 0\n\
      \\n\
      \    -- a\n\
      \    , 1\n\
      \    ]\n\
      \"
    ),
    ( "trailing line comment in multi-line list, on same line",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    [ 0 -- a\n\
      \    ]\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    [ 0 -- a\n\
      \    ]\n\
      \"
    ),
    ( "trailing line comment in multi-line record, on same line",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    { a = 1 -- a\n\
      \    }\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    { a = 1 -- a\n\
      \    }\n\
      \"
    ),
    ( "trailing line comment in multi-line record, on next line",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    { a =\n\
      \        1\n\
      \\n\
      \    -- a\n\
      \    }\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    { a =\n\
      \        1\n\
      \\n\
      \    -- a\n\
      \    }\n\
      \"
    ),
    ( "module declaration not called X",
      "module Y exposing (y)\n\
      \\n\
      \\n\
      \y =\n\
      \    0\n\
      \",
      "module Y exposing (y)\n\
      \\n\
      \\n\
      \y =\n\
      \    0\n\
      \"
    ),
    ( "module with two items in it, both exposed",
      "module X exposing (x, y)\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \\n\
      \\n\
      \y =\n\
      \    0\n\
      \",
      "module X exposing (x, y)\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \\n\
      \\n\
      \y =\n\
      \    0\n\
      \"
    ),
    ( "module with two items in it, both exposed, multiline",
      "module X exposing\n\
      \    ( x\n\
      \    , y\n\
      \    )\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \\n\
      \\n\
      \y =\n\
      \    0\n\
      \",
      "module X exposing\n\
      \    ( x\n\
      \    , y\n\
      \    )\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \\n\
      \\n\
      \y =\n\
      \    0\n\
      \"
    ),
    ( "module docs",
      "module X exposing (x)\n\
      \\n\
      \{-| x\n\
      \-}\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \{-| x\n\
      \-}\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \"
    ),
    ( "simple at docs",
      "module X exposing (x, y)\n\
      \\n\
      \{-|\n\
      \\n\
      \@docs x\n\
      \\n\
      \-}\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \\n\
      \\n\
      \y =\n\
      \    0\n\
      \",
      "module X exposing\n\
      \    ( x\n\
      \    , y\n\
      \    )\n\
      \\n\
      \{-|\n\
      \\n\
      \@docs x\n\
      \\n\
      \-}\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \\n\
      \\n\
      \y =\n\
      \    0\n\
      \"
    ),
    ( "simple type signature, formatted",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x : Int\n\
      \x =\n\
      \    0\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x : Int\n\
      \x =\n\
      \    0\n\
      \"
    ),
    ( "function parameters, formatted",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x a =\n\
      \    a\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x a =\n\
      \    a\n\
      \"
    ),
    ( "multi-item type signature, formatted",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x : Int -> Int\n\
      \x a =\n\
      \    a\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x : Int -> Int\n\
      \x a =\n\
      \    a\n\
      \"
    ),
    ( "custom type, formatted",
      "module X exposing (X(..))\n\
      \\n\
      \\n\
      \type X\n\
      \    = Y\n\
      \    | Z\n\
      \",
      "module X exposing (X(..))\n\
      \\n\
      \\n\
      \type X\n\
      \    = Y\n\
      \    | Z\n\
      \"
    ),
    ( "custom type with argument, formatted",
      "module X exposing (X(..))\n\
      \\n\
      \\n\
      \type X\n\
      \    = Y Int\n\
      \    | Z\n\
      \",
      "module X exposing (X(..))\n\
      \\n\
      \\n\
      \type X\n\
      \    = Y Int\n\
      \    | Z\n\
      \"
    ),
    ( "type alias, formatted",
      "module X exposing (X)\n\
      \\n\
      \\n\
      \type alias X =\n\
      \    Int\n\
      \",
      "module X exposing (X)\n\
      \\n\
      \\n\
      \type alias X =\n\
      \    Int\n\
      \"
    ),
    ( "type alias with parameter, formatted",
      "module X exposing (X)\n\
      \\n\
      \\n\
      \type alias X a =\n\
      \    Maybe a\n\
      \",
      "module X exposing (X)\n\
      \\n\
      \\n\
      \type alias X a =\n\
      \    Maybe a\n\
      \"
    ),
    ( "empty record type, formatted",
      "module X exposing (X)\n\
      \\n\
      \\n\
      \type alias X =\n\
      \    {}\n\
      \",
      "module X exposing (X)\n\
      \\n\
      \\n\
      \type alias X =\n\
      \    {}\n\
      \"
    ),
    ( "singleton record type, formatted",
      "module X exposing (X)\n\
      \\n\
      \\n\
      \type alias X =\n\
      \    { x : Int }\n\
      \",
      "module X exposing (X)\n\
      \\n\
      \\n\
      \type alias X =\n\
      \    { x : Int }\n\
      \"
    ),
    ( "multi-line record type, formatted",
      "module X exposing (X)\n\
      \\n\
      \\n\
      \type alias X =\n\
      \    { x : Int\n\
      \    , y : String\n\
      \    }\n\
      \",
      "module X exposing (X)\n\
      \\n\
      \\n\
      \type alias X =\n\
      \    { x : Int\n\
      \    , y : String\n\
      \    }\n\
      \"
    ),
    ( "function documentation",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \y =\n\
      \    0\n\
      \\n\
      \\n\
      \{-| x\n\
      \-}\n\
      \x =\n\
      \    2\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \y =\n\
      \    0\n\
      \\n\
      \\n\
      \{-| x\n\
      \-}\n\
      \x =\n\
      \    2\n\
      \"
    ),
    ( "simple single module import",
      "module X exposing (x)\n\
      \\n\
      \import A\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \import A\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \"
    ),
    ( "simple multiple module import",
      "module X exposing (x)\n\
      \\n\
      \import A\n\
      \import B\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \import A\n\
      \import B\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \"
    ),
    ( "sorting module imports",
      "module X exposing (x)\n\
      \\n\
      \import B\n\
      \import A\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \import A\n\
      \import B\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \"
    ),
    ( "module import alias",
      "module X exposing (x)\n\
      \\n\
      \import A as B\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \import A as B\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \"
    ),
    ( "module import single-line exposing",
      "module X exposing (x)\n\
      \\n\
      \import A as B exposing (C)\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \import A as B exposing (C)\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \"
    ),
    ( "module import multi-line exposing",
      "module X exposing (x)\n\
      \\n\
      \import A as B exposing (C,\n\
      \  D)\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \import A as B\n\
      \    exposing\n\
      \        ( C\n\
      \        , D\n\
      \        )\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \"
    ),
    ( "module import exposing type constructor",
      "module X exposing (x)\n\
      \\n\
      \import A exposing (A(B))\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \import A exposing (A(..))\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \"
    ),
    ( "argument to type",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x : List Int\n\
      \x =\n\
      \    [ 0 ]\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x : List Int\n\
      \x =\n\
      \    [ 0 ]\n\
      \"
    ),
    ( "name with a dot in it",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    a.b\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    a.b\n\
      \"
    ),
    ( "left pizza",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    a <| b\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    a <| b\n\
      \"
    ),
    ( "addition",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    1 + 2\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    1 + 2\n\
      \"
    ),
    ( "necessary parenthesis around argument",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    Just (1 + 2)\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    Just (1 + 2)\n\
      \"
    ),
    ( "a function argument indented less than the function",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    b\n\
      \   []\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    b\n\
      \        []\n\
      \"
    ),
    ( "documentation on a type declaration",
      "module X exposing (X)\n\
      \\n\
      \import A\n\
      \\n\
      \\n\
      \{-| type documentation\n\
      \-}\n\
      \type alias X =\n\
      \    Int\n\
      \",
      "module X exposing (X)\n\
      \\n\
      \import A\n\
      \\n\
      \\n\
      \{-| type documentation\n\
      \-}\n\
      \type alias X =\n\
      \    Int\n\
      \"
    ),
    ( "number in name",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x1 =\n\
      \    0\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x1 =\n\
      \    0\n\
      \"
    ),
    ( "right pizza",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    a |> b\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    a |> b\n\
      \"
    ),
    ( "function call with parenthesised argument passed to right pizza",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    a (b c)\n\
      \        |> e\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    a (b c)\n\
      \        |> e\n\
      \"
    ),
    ( "anonymous function",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    \\a -> a\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    \\a -> a\n\
      \"
    ),
    ( "left pizza and anonymous function",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    a <| \\() -> b\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    a <| \\() -> b\n\
      \"
    ),
    ( "tuple type",
      "module X exposing (X)\n\
      \\n\
      \\n\
      \type alias X =\n\
      \    ( Int, Int )\n\
      \",
      "module X exposing (X)\n\
      \\n\
      \\n\
      \type alias X =\n\
      \    ( Int, Int )\n\
      \"
    ),
    ( "unicode escape",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    \"\\u{0000}\"\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    \"\\u{0000}\"\n\
      \"
    ),
    ( "unicode escape in triple-quoted string",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    \"\"\"\\u{0000}\"\"\"\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    \"\"\"\\u{0000}\"\"\"\n\
      \"
    ),
    ( "newline literal in string",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    \"\\n\"\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    \"\\n\"\n\
      \"
    ),
    ( "newline literal in triple-quoted string",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    \"\"\"\\n\"\"\"\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    \"\"\"\\n\"\"\"\n\
      \"
    ),
    ( "deeply nested expression",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    A B B B B B B B B B\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    A B B B B B B B B B\n\
      \"
    ),
    ( "very deeply nested expression",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    A B B B B B B B B B B B B B B B B B\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    A B B B B B B B B B B B B B B B B B\n\
      \"
    ),
    ( "very deeply nested parentheses",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    A (B (B (B (B (B (B (B (B (B (B (B (B (B (B (B (B (B 0)))))))))))))))))\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    A (B (B (B (B (B (B (B (B (B (B (B (B (B (B (B (B (B 0)))))))))))))))))\n\
      \"
    ),
    ( "tuple in case pattern argument",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    case a of\n\
      \        Ok ( b, c ) ->\n\
      \            0\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    case a of\n\
      \        Ok ( b, c ) ->\n\
      \            0\n\
      \"
    ),
    ( "underscore in tuple pattern",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    case a of\n\
      \        Ok ( _, r ) ->\n\
      \            Just r\n\
      \\n\
      \        _ ->\n\
      \            Nothing\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    case a of\n\
      \        Ok ( _, r ) ->\n\
      \            Just r\n\
      \\n\
      \        _ ->\n\
      \            Nothing\n\
      \"
    ),
    ( "hyphen in module documentation",
      "module X exposing (x)\n\
      \\n\
      \{-| -\n\
      \-}\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \{-| -}\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \"
    ),
    ( "module at docs and freestyle",
      "module X exposing\n\
      \    ( x\n\
      \    , y\n\
      \    )\n\
      \\n\
      \{-|\n\
      \\n\
      \@docs x\n\
      \@docs y\n\
      \\n\
      \abc\n\
      \\n\
      \-}\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \",
      "module X exposing\n\
      \    ( x\n\
      \    , y\n\
      \    )\n\
      \\n\
      \{-|\n\
      \\n\
      \@docs x\n\
      \@docs y\n\
      \\n\
      \abc\n\
      \\n\
      \-}\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \"
    ),
    ( "case of pattern called 'co'",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    case co of\n\
      \        a ->\n\
      \            0\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    case co of\n\
      \        a ->\n\
      \            0\n\
      \"
    ),
    ( "top-level type signature following type declaration",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \type A\n\
      \    = A\n\
      \\n\
      \\n\
      \x : Int\n\
      \x =\n\
      \    0\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \type A\n\
      \    = A\n\
      \\n\
      \\n\
      \x : Int\n\
      \x =\n\
      \    0\n\
      \"
    ),
    ( "string concatenation operator: ++",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    \"a\" ++ \"b\"\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    \"a\" ++ \"b\"\n\
      \"
    ),
    ( "case expression with infix in first branch",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    case a of\n\
      \        A ->\n\
      \            y <| z\n\
      \\n\
      \        B ->\n\
      \            0\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    case a of\n\
      \        A ->\n\
      \            y <| z\n\
      \\n\
      \        B ->\n\
      \            0\n\
      \"
    ),
    ( "left-to-right pipe operator: >>",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    a >> b\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    a >> b\n\
      \"
    ),
    ( "deeply nested records",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    { a = { a = { a = { a = { a = { a = { a = { a = { a = { a = { a = { a = { a = { a = { a = { a = { a = { a = 0 } } } } } } } } } } } } } } } } } }\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    { a = { a = { a = { a = { a = { a = { a = { a = { a = { a = { a = { a = { a = { a = { a = { a = { a = { a = 0 } } } } } } } } } } } } } } } } } }\n\
      \"
    ),
    ( "equality operator: ==",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    0 == 1\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    0 == 1\n\
      \"
    ),
    ( "infix operator in brackets",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    (==)\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    (==)\n\
      \"
    ),
    ( "infix in brackets as function argument",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    a (==)\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    a (==)\n\
      \"
    ),
    ( "infix in brackets combined with ordinary infix",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    a >> (==) b\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    a >> (==) b\n\
      \"
    ),
    ( "function call in case of expression",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    case a b of\n\
      \        C ->\n\
      \            d\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    case a b of\n\
      \        C ->\n\
      \            d\n\
      \"
    ),
    ( "doc comment before custom type declaration",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \\n\
      \\n\
      \{-| A\n\
      \-}\n\
      \type A\n\
      \    = A\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \\n\
      \\n\
      \{-| A\n\
      \-}\n\
      \type A\n\
      \    = A\n\
      \"
    ),
    ( "pass infixed expression to right pizza",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    (a + b)\n\
      \        |> c\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    (a + b)\n\
      \        |> c\n\
      \"
    ),
    ( "type signature inside let in expression",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    let\n\
      \        a : Int\n\
      \        a =\n\
      \            0\n\
      \    in\n\
      \    a\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    let\n\
      \        a : Int\n\
      \        a =\n\
      \            0\n\
      \    in\n\
      \    a\n\
      \"
    ),
    ( "type with parameters in let-in type signature",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    let\n\
      \        a : List String\n\
      \        a =\n\
      \            []\n\
      \    in\n\
      \    a\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    let\n\
      \        a : List String\n\
      \        a =\n\
      \            []\n\
      \    in\n\
      \    a\n\
      \"
    ),
    ( "case of with let in expression inside first branch",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    case a of\n\
      \        Just b ->\n\
      \            let\n\
      \                c =\n\
      \                    d\n\
      \            in\n\
      \            e\n\
      \\n\
      \        Nothing ->\n\
      \            f\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    case a of\n\
      \        Just b ->\n\
      \            let\n\
      \                c =\n\
      \                    d\n\
      \            in\n\
      \            e\n\
      \\n\
      \        Nothing ->\n\
      \            f\n\
      \"
    ),
    ( "pattern alias",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x (( a, b ) as c) =\n\
      \    c\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x (( a, b ) as c) =\n\
      \    c\n\
      \"
    ),
    ( "function call in let-in bind",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    let\n\
      \        a = b 0\n\
      \    in\n\
      \    a\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    let\n\
      \        a =\n\
      \            b 0\n\
      \    in\n\
      \    a\n\
      \"
    ),
    ( "cons operator: (::)",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    0 :: []\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    0 :: []\n\
      \"
    ),
    ( "type signature after function call in let in expression",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    let\n\
      \        a =\n\
      \            b c\n\
      \\n\
      \        d : Int\n\
      \        d =\n\
      \            0\n\
      \    in\n\
      \    d\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    let\n\
      \        a =\n\
      \            b c\n\
      \\n\
      \        d : Int\n\
      \        d =\n\
      \            0\n\
      \    in\n\
      \    d\n\
      \"
    ),
    ( "line comment inside case of branch",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    case a of\n\
      \        b ->\n\
      \            -- c\n\
      \            d\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    case a of\n\
      \        b ->\n\
      \            -- c\n\
      \            d\n\
      \"
    ),
    ( "function argument in function type signature",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x : (Int -> Int) -> Int\n\
      \x =\n\
      \    0\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x : (Int -> Int) -> Int\n\
      \x =\n\
      \    0\n\
      \"
    ),
    ( "list cons in case of pattern",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    case a of\n\
      \        b :: c ->\n\
      \            d\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    case a of\n\
      \        b :: c ->\n\
      \            d\n\
      \"
    ),
    ( "pattern with alias and list cons",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    case a of\n\
      \        ((B c) as d) :: e ->\n\
      \            f\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    case a of\n\
      \        ((B c) as d) :: e ->\n\
      \            f\n\
      \"
    ),
    ( "function call between 'if' and 'else'",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    if a b then\n\
      \        c\n\
      \\n\
      \    else\n\
      \        d\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    if a b then\n\
      \        c\n\
      \\n\
      \    else\n\
      \        d\n\
      \"
    ),
    ( "line comment at the top of a top-level bind expression",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    -- a\n\
      \    b\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    -- a\n\
      \    b\n\
      \"
    ),
    ( "line comment at top of else branch",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    if a then\n\
      \        b\n\
      \\n\
      \    else\n\
      \        -- c\n\
      \        d\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    if a then\n\
      \        b\n\
      \\n\
      \    else\n\
      \        -- c\n\
      \        d\n\
      \"
    ),
    ( "function call in then part of if-then-else",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    if a then\n\
      \        b c\n\
      \\n\
      \    else\n\
      \        d\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    if a then\n\
      \        b c\n\
      \\n\
      \    else\n\
      \        d\n\
      \"
    ),
    ( "comment before infix operator",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    a\n\
      \        -- b\n\
      \        |> c\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    a\n\
      \        -- b\n\
      \        |> c\n\
      \"
    ),
    ( "aliased function type",
      "module X exposing (X)\n\
      \\n\
      \\n\
      \type alias X =\n\
      \    A -> B\n\
      \",
      "module X exposing (X)\n\
      \\n\
      \\n\
      \type alias X =\n\
      \    A -> B\n\
      \"
    ),
    ( "declaration after type alias",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \type alias A =\n\
      \    B\n\
      \\n\
      \\n\
      \x =\n\
      \    d\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \type alias A =\n\
      \    B\n\
      \\n\
      \\n\
      \x =\n\
      \    d\n\
      \"
    ),
    ( "case expression inside infix expression",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    a <|\n\
      \        case b of\n\
      \            C ->\n\
      \                d\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    a <|\n\
      \        case b of\n\
      \            C ->\n\
      \                d\n\
      \"
    ),
    ( "record destructuring pattern",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x { a } =\n\
      \    a\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x { a } =\n\
      \    a\n\
      \"
    ),
    ( "record destructuring inside function argument",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    case a of\n\
      \        B { c } ->\n\
      \            d\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    case a of\n\
      \        B { c } ->\n\
      \            d\n\
      \"
    ),
    ( "greater than symbol",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    0 > 1\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    0 > 1\n\
      \"
    ),
    ( "less than symbol",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    0 < 1\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    0 < 1\n\
      \"
    ),
    ( "name beginning with let",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x letA =\n\
      \    letA\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x letA =\n\
      \    letA\n\
      \"
    ),
    ( "name beginning with if",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x ifA =\n\
      \    ifA\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x ifA =\n\
      \    ifA\n\
      \"
    ),
    ( "name beginning with case",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x caseA =\n\
      \    caseA\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x caseA =\n\
      \    caseA\n\
      \"
    ),
    ( "if statement in case branch",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    case a of\n\
      \        B ->\n\
      \            if c then\n\
      \                d\n\
      \\n\
      \            else\n\
      \                e\n\
      \\n\
      \        F ->\n\
      \            g\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    case a of\n\
      \        B ->\n\
      \            if c then\n\
      \                d\n\
      \\n\
      \            else\n\
      \                e\n\
      \\n\
      \        F ->\n\
      \            g\n\
      \"
    ),
    ( "char literal",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    'a'\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    'a'\n\
      \"
    ),
    ( "tab char literal",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    '\\t'\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    '\\t'\n\
      \"
    ),
    ( "logical or operator (||)",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    a || b\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    a || b\n\
      \"
    ),
    ( "single quote in char literal",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    '\\''\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    '\\''\n\
      \"
    ),
    ( "top-level line comment",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \a =\n\
      \    0\n\
      \\n\
      \\n\
      \\n\
      \-- s\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \a =\n\
      \    0\n\
      \\n\
      \\n\
      \\n\
      \-- s\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \"
    ),
    ( "section comment at the top of the module",
      "module X exposing (x)\n\
      \\n\
      \-- a\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \-- a\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \"
    ),
    ( "declaration followed by line comment and documented declaration",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \a =\n\
      \    0\n\
      \\n\
      \\n\
      \\n\
      \-- a\n\
      \\n\
      \\n\
      \{-| a\n\
      \-}\n\
      \x =\n\
      \    0\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \a =\n\
      \    0\n\
      \\n\
      \\n\
      \\n\
      \-- a\n\
      \\n\
      \\n\
      \{-| a\n\
      \-}\n\
      \x =\n\
      \    0\n\
      \"
    ),
    ( "string literal pattern",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    case a of\n\
      \        \"b\" ->\n\
      \            c\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    case a of\n\
      \        \"b\" ->\n\
      \            c\n\
      \"
    ),
    ( "type declaration with parameters",
      "module X exposing (X)\n\
      \\n\
      \\n\
      \type X a\n\
      \    = X\n\
      \",
      "module X exposing (X)\n\
      \\n\
      \\n\
      \type X a\n\
      \    = X\n\
      \"
    ),
    ( "anonymous function with argument",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    (\\a -> a) b\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    (\\a -> a) b\n\
      \"
    ),
    ( "multiple binds in let in expression in anonymous function",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    \\a ->\n\
      \        let\n\
      \            b =\n\
      \                c\n\
      \\n\
      \            d =\n\
      \                e\n\
      \        in\n\
      \        f\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    \\a ->\n\
      \        let\n\
      \            b =\n\
      \                c\n\
      \\n\
      \            d =\n\
      \                e\n\
      \        in\n\
      \        f\n\
      \"
    ),
    ( "char literal as function argument",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    a ','\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    a ','\n\
      \"
    ),
    ( "not equal infix operator",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    (/=)\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    (/=)\n\
      \"
    ),
    ( "import infix",
      "module X exposing (x)\n\
      \\n\
      \import A exposing ((==))\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \import A exposing ((==))\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \"
    ),
    ( "parser infix (|.)",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    (|.)\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    (|.)\n\
      \"
    ),
    ( "parser infix (|=)",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    (|=)\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    (|=)\n\
      \"
    ),
    ( "unicode code point in char literal",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    '\\u{000D}'\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    '\\u{000D}'\n\
      \"
    ),
    ( "logical and operator (&&)",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    (&&)\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    (&&)\n\
      \"
    ),
    ( "name beginning with import",
      "module X exposing (importX)\n\
      \\n\
      \\n\
      \importX =\n\
      \    0\n\
      \",
      "module X exposing (importX)\n\
      \\n\
      \\n\
      \importX =\n\
      \    0\n\
      \"
    ),
    ( "minus infix operator (-)",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    a - b\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    a - b\n\
      \"
    ),
    ( "function call followed by section comment",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    a b\n\
      \\n\
      \\n\
      \\n\
      \-- c\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    a b\n\
      \\n\
      \\n\
      \\n\
      \-- c\n\
      \"
    ),
    ( "closing bracket inside string literal",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    [ \"]\"\n\
      \    ]\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    [ \"]\"\n\
      \    ]\n\
      \"
    ),
    ( "function type as argument to type",
      "module X exposing (X)\n\
      \\n\
      \\n\
      \type X\n\
      \    = X (A -> B)\n\
      \",
      "module X exposing (X)\n\
      \\n\
      \\n\
      \type X\n\
      \    = X (A -> B)\n\
      \"
    ),
    ( "record type as type parameter",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \type A\n\
      \    = A { b : C }\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \type A\n\
      \    = A { b : C }\n\
      \"
    ),
    ( "pattern containing type function call and list cons",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    case a of\n\
      \        (B C) :: _ ->\n\
      \            d\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    case a of\n\
      \        (B C) :: _ ->\n\
      \            d\n\
      \"
    ),
    ( "record lookup on parenthesised expression",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    (a b).c\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    (a b).c\n\
      \"
    ),
    ( "combination of ++ and >>",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    (++) >> a\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    (++) >> a\n\
      \"
    ),
    ( "combination of record and type pattern in function argument",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x (A { b }) =\n\
      \    c\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x (A { b }) =\n\
      \    c\n\
      \"
    ),
    ( "record literal in infix expression",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    {} |> a\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    {} |> a\n\
      \"
    ),
    ( "tuple and name patterns in anonymous function arguments",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    \\( a, b ) c -> d\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    \\( a, b ) c -> d\n\
      \"
    ),
    ( "tab literal inside simple string literal",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    \"\\t\"\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    \"\\t\"\n\
      \"
    ),
    ( "let expression in infix expression",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    a <|\n\
      \        let\n\
      \            b =\n\
      \                c\n\
      \        in\n\
      \        d\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    a <|\n\
      \        let\n\
      \            b =\n\
      \                c\n\
      \        in\n\
      \        d\n\
      \"
    ),
    ( "two double quotes inside triple-quoted string",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    \"\"\" \"\" \"\"\"\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    \"\"\" \"\" \"\"\"\n\
      \"
    ),
    ( "multiplication infix operator *",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    a * b\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    a * b\n\
      \"
    ),
    ( "exponentiation operator ^",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    a ^ b\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    a ^ b\n\
      \"
    ),
    ( "name beginning with case in infix expression",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    caseA |> b\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    caseA |> b\n\
      \"
    ),
    ( "name beginning with let in infix expression",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    letA |> b\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    letA |> b\n\
      \"
    ),
    ( "multi-line function call with first argument on first line",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    a b\n\
      \        c\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    a b\n\
      \        c\n\
      \"
    ),
    ( "multiline function call inside a list",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    [ a\n\
      \        b\n\
      \    ]\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    [ a\n\
      \        b\n\
      \    ]\n\
      \"
    ),
    ( "multiline infix expression inside a list",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    [ a <|\n\
      \        b\n\
      \    ]\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    [ a <|\n\
      \        b\n\
      \    ]\n\
      \"
    ),
    ( "anonymous function inside a list",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    [ \\a ->\n\
      \        b\n\
      \    ]\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    [ \\a ->\n\
      \        b\n\
      \    ]\n\
      \"
    ),
    ( "anonymous function in infix in list in function call",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    a\n\
      \        [ b <|\n\
      \            \\() -> c\n\
      \        ]\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    a\n\
      \        [ b <|\n\
      \            \\() -> c\n\
      \        ]\n\
      \"
    ),
    ( "left pizza in record literal",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    { a =\n\
      \        b <|\n\
      \            c\n\
      \    }\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    { a =\n\
      \        b <|\n\
      \            c\n\
      \    }\n\
      \"
    ),
    ( "multiline infix as function argument",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    a\n\
      \        (b\n\
      \            ++ c\n\
      \        )\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    a\n\
      \        (b\n\
      \            ++ c\n\
      \        )\n\
      \"
    ),
    ( "multiline record in function argument in list",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    [ a\n\
      \        { b = c\n\
      \        , d = f\n\
      \        }\n\
      \    ]\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    [ a\n\
      \        { b = c\n\
      \        , d = f\n\
      \        }\n\
      \    ]\n\
      \"
    ),
    ( "single line record field in multi line record",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    { a = b c\n\
      \    }\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    { a = b c\n\
      \    }\n\
      \"
    ),
    ( "multiline list inside record",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    { a =\n\
      \        [ b\n\
      \        ]\n\
      \    }\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    { a =\n\
      \        [ b\n\
      \        ]\n\
      \    }\n\
      \"
    ),
    ( "multiline list in infix in infix",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    (a <|\n\
      \        [ b\n\
      \        , c\n\
      \        ]\n\
      \    )\n\
      \        |> d\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    (a <|\n\
      \        [ b\n\
      \        , c\n\
      \        ]\n\
      \    )\n\
      \        |> d\n\
      \"
    ),
    ( "multiline list in function call in function call",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    a\n\
      \        (b\n\
      \            [ c\n\
      \            , d\n\
      \            ]\n\
      \        )\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    a\n\
      \        (b\n\
      \            [ c\n\
      \            , d\n\
      \            ]\n\
      \        )\n\
      \"
    ),
    ( "multiline function call in let binding",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    let\n\
      \        a =\n\
      \            b\n\
      \                c\n\
      \    in\n\
      \    d\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    let\n\
      \        a =\n\
      \            b\n\
      \                c\n\
      \    in\n\
      \    d\n\
      \"
    ),
    ( "single line record field in multiline record in list",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    [ { a = b\n\
      \      }\n\
      \    ]\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    [ { a = b\n\
      \      }\n\
      \    ]\n\
      \"
    ),
    ( "double multiline infix",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    a <|\n\
      \        b <|\n\
      \            c\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    a <|\n\
      \        b <|\n\
      \            c\n\
      \"
    ),
    ( "multiline string in infix",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    \"\"\"\n\
      \\"\"\" |> a\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    \"\"\"\n\
      \\"\"\" |> a\n\
      \"
    ),
    ( "multiline record field in tuple",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    ( a\n\
      \    , { b =\n\
      \            c\n\
      \      }\n\
      \    )\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    ( a\n\
      \    , { b =\n\
      \            c\n\
      \      }\n\
      \    )\n\
      \"
    ),
    ( "infix in multiline record field in tuple",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    ( a\n\
      \    , { b =\n\
      \            c <|\n\
      \                d\n\
      \      }\n\
      \    )\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    ( a\n\
      \    , { b =\n\
      \            c <|\n\
      \                d\n\
      \      }\n\
      \    )\n\
      \"
    ),
    ( "multiline record in tuple",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    ( a\n\
      \    , { b = c\n\
      \      , d = e\n\
      \      }\n\
      \    )\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    ( a\n\
      \    , { b = c\n\
      \      , d = e\n\
      \      }\n\
      \    )\n\
      \"
    ),
    ( "multiline function call in infix",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    a\n\
      \        b\n\
      \    <|\n\
      \        c\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    a\n\
      \        b\n\
      \    <|\n\
      \        c\n\
      \"
    ),
    ( "name ending with underscore",
      "module X exposing (x_)\n\
      \\n\
      \\n\
      \x_ =\n\
      \    0\n\
      \",
      "module X exposing (x_)\n\
      \\n\
      \\n\
      \x_ =\n\
      \    0\n\
      \"
    ),
    ( "multiline string in multiline infix",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    \"\"\"\n\
      \\"\"\"\n\
      \        |> a\n\
      \        |> b\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    \"\"\"\n\
      \\"\"\"\n\
      \        |> a\n\
      \        |> b\n\
      \"
    ),
    ( "multiline record field in anonymous function in list",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    [ \\() ->\n\
      \        { a =\n\
      \            b\n\
      \        }\n\
      \    ]\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    [ \\() ->\n\
      \        { a =\n\
      \            b\n\
      \        }\n\
      \    ]\n\
      \"
    ),
    ( "multiline tuple in infix in list",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    [ a <|\n\
      \        ( b\n\
      \        , c\n\
      \        )\n\
      \    ]\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    [ a <|\n\
      \        ( b\n\
      \        , c\n\
      \        )\n\
      \    ]\n\
      \"
    ),
    ( "multiline string in function argument",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    a \"\"\"\n\
      \\"\"\"\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    a \"\"\"\n\
      \\"\"\"\n\
      \"
    ),
    ( "string in function argument in function argument on new line",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    a\n\
      \        (b \"\"\"\n\
      \\"\"\")\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    a\n\
      \        (b \"\"\"\n\
      \\"\"\")\n\
      \"
    ),
    ( "multiple trailing line comments in list",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    [ a\n\
      \\n\
      \    --\n\
      \    --\n\
      \    ]\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    [ a\n\
      \\n\
      \    --\n\
      \    --\n\
      \    ]\n\
      \"
    ),
    ( "let bind in list",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    [ let\n\
      \        a =\n\
      \            b\n\
      \      in\n\
      \      a\n\
      \    ]\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    [ let\n\
      \        a =\n\
      \            b\n\
      \      in\n\
      \      a\n\
      \    ]\n\
      \"
    ),
    ( "let in expression in infix in list",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    [ a <|\n\
      \        let\n\
      \            b =\n\
      \                c\n\
      \        in\n\
      \        d\n\
      \    ]\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    [ a <|\n\
      \        let\n\
      \            b =\n\
      \                c\n\
      \        in\n\
      \        d\n\
      \    ]\n\
      \"
    ),
    ( "multiline tuple in list",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    [ ( a\n\
      \      , b\n\
      \      )\n\
      \    ]\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    [ ( a\n\
      \      , b\n\
      \      )\n\
      \    ]\n\
      \"
    ),
    ( "single line export with @docs",
      "module X exposing (x)\n\
      \\n\
      \{-|\n\
      \\n\
      \@docs a\n\
      \\n\
      \-}\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \{-|\n\
      \\n\
      \@docs a\n\
      \\n\
      \-}\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \"
    ),
    ( "two documented exports in a single line",
      "module X exposing (a, b)\n\
      \\n\
      \{-|\n\
      \\n\
      \@docs a, b\n\
      \\n\
      \-}\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \",
      "module X exposing (a, b)\n\
      \\n\
      \{-|\n\
      \\n\
      \@docs a, b\n\
      \\n\
      \-}\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \"
    ),
    ( "text at top of module documentation",
      "module X exposing (x)\n\
      \\n\
      \{-| a\n\
      \\n\
      \@docs x\n\
      \\n\
      \-}\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \{-| a\n\
      \\n\
      \@docs x\n\
      \\n\
      \-}\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \"
    ),
    ( "elm comment in code block in module doc comment",
      "module X exposing (x)\n\
      \\n\
      \{-|\n\
      \\n\
      \\n\
      \\n\
      \    {- a -}\n\
      \\n\
      \-}\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \{-|\n\
      \\n\
      \\n\
      \\n\
      \    {- a -}\n\
      \\n\
      \-}\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \"
    ),
    ( "case of expression in list",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    [ case a of\n\
      \        B ->\n\
      \            c\n\
      \\n\
      \        D ->\n\
      \            e\n\
      \    ]\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    [ case a of\n\
      \        B ->\n\
      \            c\n\
      \\n\
      \        D ->\n\
      \            e\n\
      \    ]\n\
      \"
    ),
    ( "export docs",
      "module X exposing\n\
      \    ( a, b\n\
      \    , c\n\
      \    )\n\
      \\n\
      \{-|\n\
      \\n\
      \@docs a, b\n\
      \\n\
      \-}\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \",
      "module X exposing\n\
      \    ( a, b\n\
      \    , c\n\
      \    )\n\
      \\n\
      \{-|\n\
      \\n\
      \@docs a, b\n\
      \\n\
      \-}\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \"
    ),
    ( "module docs with expose all on type export",
      "module X exposing\n\
      \    ( B(..), c\n\
      \    , d\n\
      \    )\n\
      \\n\
      \{-|\n\
      \\n\
      \@docs B, c\n\
      \\n\
      \-}\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \",
      "module X exposing\n\
      \    ( B(..), c\n\
      \    , d\n\
      \    )\n\
      \\n\
      \{-|\n\
      \\n\
      \@docs B, c\n\
      \\n\
      \-}\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \"
    ),
    ( "documented exports with hyphen in code quote in module docs",
      "module X exposing\n\
      \    ( a, b\n\
      \    , c\n\
      \    )\n\
      \\n\
      \{-|\n\
      \\n\
      \    -\n\
      \\n\
      \@docs a, b\n\
      \\n\
      \-}\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \",
      "module X exposing\n\
      \    ( a, b\n\
      \    , c\n\
      \    )\n\
      \\n\
      \{-|\n\
      \\n\
      \    -\n\
      \\n\
      \@docs a, b\n\
      \\n\
      \-}\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \"
    ),
    ( "8 space indent inside module documentation",
      "module X exposing (x)\n\
      \\n\
      \{-|\n\
      \\n\
      \        a\n\
      \\n\
      \-}\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \{-|\n\
      \\n\
      \        a\n\
      \\n\
      \-}\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \"
    ),
    ( "hyphen in word in module documentation",
      "module X exposing (x)\n\
      \\n\
      \{-| a-\n\
      \-}\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \{-| a-\n\
      \-}\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \"
    ),
    ( "chained if then else expressions",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    if a then\n\
      \        b\n\
      \\n\
      \    else if c then\n\
      \        d\n\
      \\n\
      \    else\n\
      \        e\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    if a then\n\
      \        b\n\
      \\n\
      \    else if c then\n\
      \        d\n\
      \\n\
      \    else\n\
      \        e\n\
      \"
    ),
    ( "function call in record lookup in function call",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    a (b c).d\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    a (b c).d\n\
      \"
    ),
    ( "pass record access function to function",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    a .b\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    a .b\n\
      \"
    ),
    ( "function call in dotted expression in argument to anonymous function",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    (\\a -> a) (b c).d\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    (\\a -> a) (b c).d\n\
      \"
    ),
    ( "line comment at start of subsequent list item",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    [ a\n\
      \    , -- b\n\
      \      c\n\
      \    ]\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    [ a\n\
      \    , -- b\n\
      \      c\n\
      \    ]\n\
      \"
    ),
    ( "multiline record type declaration in custom type declaration",
      "module X exposing (X)\n\
      \\n\
      \\n\
      \type X\n\
      \    = X\n\
      \        { a : A\n\
      \        }\n\
      \",
      "module X exposing (X)\n\
      \\n\
      \\n\
      \type X\n\
      \    = X\n\
      \        { a : A\n\
      \        }\n\
      \"
    ),
    ( "type declaration followed by function declaration",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \type A\n\
      \    = B C\n\
      \\n\
      \\n\
      \x =\n\
      \    e\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \type A\n\
      \    = B C\n\
      \\n\
      \\n\
      \x =\n\
      \    e\n\
      \"
    ),
    ( "tuple type in type signature",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x : ( A, B )\n\
      \x =\n\
      \    c\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x : ( A, B )\n\
      \x =\n\
      \    c\n\
      \"
    ),
    ( "record update in function argument",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    a { b | c = d }\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    a { b | c = d }\n\
      \"
    ),
    ( "record update in infix expression",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    a <| { b | c = d }\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    a <| { b | c = d }\n\
      \"
    ),
    ( "tuple type in type parameter",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x : A ( B, C )\n\
      \x =\n\
      \    d\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x : A ( B, C )\n\
      \x =\n\
      \    d\n\
      \"
    ),
    ( "simple and function parameters in custom type branch",
      "module X exposing (X)\n\
      \\n\
      \\n\
      \type X\n\
      \    = A B (C D)\n\
      \",
      "module X exposing (X)\n\
      \\n\
      \\n\
      \type X\n\
      \    = A B (C D)\n\
      \"
    ),
    ( "if else expression in parentheses",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    a\n\
      \        (if b then\n\
      \            c\n\
      \\n\
      \         else\n\
      \            d\n\
      \        )\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    a\n\
      \        (if b then\n\
      \            c\n\
      \\n\
      \         else\n\
      \            d\n\
      \        )\n\
      \"
    ),
    ( "parenthesised destructuring in let in binding",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    let\n\
      \        (A b) =\n\
      \            c\n\
      \    in\n\
      \    d\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    let\n\
      \        (A b) =\n\
      \            c\n\
      \    in\n\
      \    d\n\
      \"
    ),
    ( "newline after colon in type alias declaration",
      "module X exposing (X)\n\
      \\n\
      \\n\
      \type alias X =\n\
      \    { a :\n\
      \        B\n\
      \    }\n\
      \",
      "module X exposing (X)\n\
      \\n\
      \\n\
      \type alias X =\n\
      \    { a :\n\
      \        B\n\
      \    }\n\
      \"
    ),
    ( "string in function argument in function argument on same line",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    a (b \"\"\"\n\
      \\"\"\")\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    a (b \"\"\"\n\
      \\"\"\")\n\
      \"
    ),
    ( "point left function composition operator <<",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    a << b\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    a << b\n\
      \"
    ),
    ( "block comment between function and argument",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    a {- b -} c\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    a {- b -} c\n\
      \"
    ),
    ( "function type in tuple in type signature",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x : ( A, B -> C )\n\
      \x =\n\
      \    d\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x : ( A, B -> C )\n\
      \x =\n\
      \    d\n\
      \"
    ),
    ( "line comment after custom type branch",
      "module X exposing (X)\n\
      \\n\
      \\n\
      \type X\n\
      \    = A\n\
      \      -- b\n\
      \    | C\n\
      \",
      "module X exposing (X)\n\
      \\n\
      \\n\
      \type X\n\
      \    = A\n\
      \      -- b\n\
      \    | C\n\
      \"
    ),
    ( "line comment before let in binding",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    let\n\
      \        -- a\n\
      \        b =\n\
      \            c\n\
      \    in\n\
      \    d\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    let\n\
      \        -- a\n\
      \        b =\n\
      \            c\n\
      \    in\n\
      \    d\n\
      \"
    ),
    ( "line comment before custom type branch",
      "module X exposing (X)\n\
      \\n\
      \\n\
      \type X\n\
      \    = -- a\n\
      \      B\n\
      \",
      "module X exposing (X)\n\
      \\n\
      \\n\
      \type X\n\
      \    = -- a\n\
      \      B\n\
      \"
    ),
    ( "record destructuring pattern in alias pattern in case of",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    case a of\n\
      \        { b } as c ->\n\
      \            d\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    case a of\n\
      \        { b } as c ->\n\
      \            d\n\
      \"
    ),
    ( "record destructuring pattern in pattern alias in application pattern",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    case a of\n\
      \        B ({ c } as d) ->\n\
      \            e\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    case a of\n\
      \        B ({ c } as d) ->\n\
      \            e\n\
      \"
    ),
    ( "block comment above right hand side of let in bind",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    let\n\
      \        a =\n\
      \            {- b -}\n\
      \            c\n\
      \    in\n\
      \    d\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    let\n\
      \        a =\n\
      \            {- b -}\n\
      \            c\n\
      \    in\n\
      \    d\n\
      \"
    ),
    ( "block comment between import keyword and name",
      "module X exposing (x)\n\
      \\n\
      \import {- a -} B\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \import {- a -} B\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \"
    ),
    ( "extensible record type",
      "module X exposing (X)\n\
      \\n\
      \\n\
      \type alias X =\n\
      \    { a\n\
      \        | b : C\n\
      \    }\n\
      \",
      "module X exposing (X)\n\
      \\n\
      \\n\
      \type alias X =\n\
      \    { a\n\
      \        | b : C\n\
      \    }\n\
      \"
    ),
    ( "multi item extensible record type",
      "module X exposing (X)\n\
      \\n\
      \\n\
      \type alias X =\n\
      \    { a\n\
      \        | b : C\n\
      \        , d : E\n\
      \    }\n\
      \",
      "module X exposing (X)\n\
      \\n\
      \\n\
      \type alias X =\n\
      \    { a\n\
      \        | b : C\n\
      \        , d : E\n\
      \    }\n\
      \"
    ),
    ( "line comment and record type in type alias",
      "module X exposing (X)\n\
      \\n\
      \\n\
      \type alias X =\n\
      \    -- a\n\
      \    { b : C }\n\
      \",
      "module X exposing (X)\n\
      \\n\
      \\n\
      \type alias X =\n\
      \    -- a\n\
      \    { b : C }\n\
      \"
    ),
    ( "line comment before record type field in type alias",
      "module X exposing (X)\n\
      \\n\
      \\n\
      \type alias X =\n\
      \    { -- a\n\
      \      b : C\n\
      \    }\n\
      \",
      "module X exposing (X)\n\
      \\n\
      \\n\
      \type alias X =\n\
      \    { -- a\n\
      \      b : C\n\
      \    }\n\
      \"
    ),
    ( "comment in then part of if then else statement",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    if a then\n\
      \        -- b\n\
      \        c\n\
      \\n\
      \    else\n\
      \        d\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    if a then\n\
      \        -- b\n\
      \        c\n\
      \\n\
      \    else\n\
      \        d\n\
      \"
    ),
    ( "integer division operator //",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    a // b\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    a // b\n\
      \"
    ),
    ( "division operator /",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    a / b\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    a / b\n\
      \"
    ),
    ( "line comment above case of branch",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    case a of\n\
      \        -- b\n\
      \        c ->\n\
      \            d\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    case a of\n\
      \        -- b\n\
      \        c ->\n\
      \            d\n\
      \"
    ),
    ( "negative int",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    -1\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    -1\n\
      \"
    ),
    ( "less than or equal to operator <=",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    a <= b\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    a <= b\n\
      \"
    ),
    ( "block comment after right pizza operator",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    a |> {- b -} c\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    a |> {- b -} c\n\
      \"
    ),
    ( "block comment before right pizza operator",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    a {- b -} |> c\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    a {- b -} |> c\n\
      \"
    ),
    ( "block comment before left pizza operator",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    a {- b -} <| c\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    a {- b -} <| c\n\
      \"
    ),
    ( "block comment after left pizza operator",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    a <| {- b -} c\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    a <| {- b -} c\n\
      \"
    ),
    ( "block comment at start of anonymous function body",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    \\a ->\n\
      \        {- b -}\n\
      \        c\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    \\a ->\n\
      \        {- b -}\n\
      \        c\n\
      \"
    ),
    ( "block comment after in in let in statement",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    let\n\
      \        a =\n\
      \            b\n\
      \    in\n\
      \    {- c -}\n\
      \    d\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    let\n\
      \        a =\n\
      \            b\n\
      \    in\n\
      \    {- c -}\n\
      \    d\n\
      \"
    ),
    ( "list concatenation with a list literal",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    [] ++ a\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    [] ++ a\n\
      \"
    ),
    ( "line comment in infix following function call",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    a b\n\
      \        -- c\n\
      \        :: d\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    a b\n\
      \        -- c\n\
      \        :: d\n\
      \"
    ),
    ( "top-level name beginning with as in module with exports",
      "module X exposing (x)\n\
      \\n\
      \import A\n\
      \\n\
      \\n\
      \asx =\n\
      \    a\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \import A\n\
      \\n\
      \\n\
      \asx =\n\
      \    a\n\
      \"
    ),
    ( "pattern with two list cons operators",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    case a of\n\
      \        b :: c :: d ->\n\
      \            e\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    case a of\n\
      \        b :: c :: d ->\n\
      \            e\n\
      \"
    ),
    ( "block comment inside record field bind",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    { a = {- b -} c }\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    { a = {- b -} c }\n\
      \"
    ),
    ( "if then else statement on the right of left pizza",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    a <|\n\
      \        if b then\n\
      \            c\n\
      \\n\
      \        else\n\
      \            d\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    a <|\n\
      \        if b then\n\
      \            c\n\
      \\n\
      \        else\n\
      \            d\n\
      \"
    ),
    ( "char literal in pattern",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    case a of\n\
      \        ' ' ->\n\
      \            b\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    case a of\n\
      \        ' ' ->\n\
      \            b\n\
      \"
    ),
    ( "function type in record type field in type signature",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x : { a : A -> B }\n\
      \x =\n\
      \    a\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x : { a : A -> B }\n\
      \x =\n\
      \    a\n\
      \"
    ),
    ( "char literal in cons pattern",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    case a of\n\
      \        'b' :: _ ->\n\
      \            c\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    case a of\n\
      \        'b' :: _ ->\n\
      \            c\n\
      \"
    ),
    ( "case of expression as callable",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    (case a of\n\
      \        b ->\n\
      \            c\n\
      \    )\n\
      \        d\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    (case a of\n\
      \        b ->\n\
      \            c\n\
      \    )\n\
      \        d\n\
      \"
    ),
    ( "block comment before main expression in case of statement",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    case {- a -} b of\n\
      \        c ->\n\
      \            d\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    case {- a -} b of\n\
      \        c ->\n\
      \            d\n\
      \"
    ),
    ( "port module",
      "port module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \",
      "port module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \"
    ),
    ( "single line port declaration",
      "port module X exposing (x)\n\
      \\n\
      \\n\
      \port x : A\n\
      \",
      "port module X exposing (x)\n\
      \\n\
      \\n\
      \port x : A\n\
      \"
    ),
    ( "top-level name beginning with port",
      "module X exposing (portX)\n\
      \\n\
      \\n\
      \portX =\n\
      \    0\n\
      \",
      "module X exposing (portX)\n\
      \\n\
      \\n\
      \portX =\n\
      \    0\n\
      \"
    ),
    ( "multiline type signature in port declaration",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \port x :\n\
      \    A\n\
      \    -> B\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \port x :\n\
      \    A\n\
      \    -> B\n\
      \"
    ),
    ( "function type in tuple in type argument in type signature",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x : A ( B, C -> D )\n\
      \x =\n\
      \    e\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x : A ( B, C -> D )\n\
      \x =\n\
      \    e\n\
      \"
    ),
    ( "type branch name at start of line",
      "module X exposing (X)\n\
      \\n\
      \\n\
      \type X\n\
      \    = A\n\
      \    |\n\
      \B\n\
      \",
      "module X exposing (X)\n\
      \\n\
      \\n\
      \type X\n\
      \    = A\n\
      \    | B\n\
      \"
    ),
    ( "multiline module title",
      "module X exposing (x)\n\
      \\n\
      \-- a\n\
      \-- b\n\
      \\n\
      \import C\n\
      \\n\
      \\n\
      \\n\
      \-- d\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \-- a\n\
      \-- b\n\
      \\n\
      \import C\n\
      \\n\
      \\n\
      \\n\
      \-- d\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \"
    ),
    ( "top-level line comment after top-level block comment",
      "module X exposing (x)\n\
      \\n\
      \{- a -}\n\
      \-- b\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \{- a -}\n\
      \-- b\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \"
    ),
    ( "section comment after import before documented type declaration",
      "module X exposing (X)\n\
      \\n\
      \import A\n\
      \\n\
      \\n\
      \\n\
      \-- b\n\
      \\n\
      \\n\
      \{-| c\n\
      \-}\n\
      \type X\n\
      \    = D\n\
      \",
      "module X exposing (X)\n\
      \\n\
      \import A\n\
      \\n\
      \\n\
      \\n\
      \-- b\n\
      \\n\
      \\n\
      \{-| c\n\
      \-}\n\
      \type X\n\
      \    = D\n\
      \"
    ),
    ( "greater than or equal to operator >=",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    a >= b\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    a >= b\n\
      \"
    ),
    ( "port with documentation",
      "port module X exposing (x)\n\
      \\n\
      \import A\n\
      \\n\
      \\n\
      \{-| a\n\
      \-}\n\
      \port x : Int\n\
      \",
      "port module X exposing (x)\n\
      \\n\
      \import A\n\
      \\n\
      \\n\
      \{-| a\n\
      \-}\n\
      \port x : Int\n\
      \"
    ),
    ( "GLSL syntax",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    [glsl|\n\
      \\n\
      \attribute vec3 position;\n\
      \attribute vec3 coord;\n\
      \uniform   mat4 view;\n\
      \varying   vec2 vcoord;\n\
      \\n\
      \void main () {\n\
      \  gl_Position = view * vec4(position, 1.0);\n\
      \  vcoord = coord.xy;\n\
      \}\n\
      \\n\
      \|]\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    [glsl|\n\
      \\n\
      \attribute vec3 position;\n\
      \attribute vec3 coord;\n\
      \uniform   mat4 view;\n\
      \varying   vec2 vcoord;\n\
      \\n\
      \void main () {\n\
      \  gl_Position = view * vec4(position, 1.0);\n\
      \  vcoord = coord.xy;\n\
      \}\n\
      \\n\
      \|]\n\
      \"
    ),
    ( "extensible record type in type parameter",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x : A { b | c : D }\n\
      \x =\n\
      \    e\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x : A { b | c : D }\n\
      \x =\n\
      \    e\n\
      \"
    ),
    ( "negative float literal",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    -1.0\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    -1.0\n\
      \"
    ),
    ( "negative zero integer literal",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    -0\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \"
    ),
    ( "negative zero hex integer literal",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    -0x00\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    0x00\n\
      \"
    ),
    ( "multiline string followed by right pizza on new line",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    \"\"\"\n\
      \\"\"\"\n\
      \        |> a\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    \"\"\"\n\
      \\"\"\"\n\
      \        |> a\n\
      \"
    ),
    ( "trailing line comment on new line in tuple",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    ( a\n\
      \    , b\n\
      \      -- c\n\
      \    )\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    ( a\n\
      \    , b\n\
      \      -- c\n\
      \    )\n\
      \"
    ),
    ( "type declaration on name beginning with port",
      "module X exposing (portX)\n\
      \\n\
      \\n\
      \portX : A\n\
      \portX =\n\
      \    0\n\
      \",
      "module X exposing (portX)\n\
      \\n\
      \\n\
      \portX : A\n\
      \portX =\n\
      \    0\n\
      \"
    ),
    ( "multiline imports",
      "module X exposing (x)\n\
      \\n\
      \import A\n\
      \    exposing\n\
      \        ( b\n\
      \        , c\n\
      \        )\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \import A\n\
      \    exposing\n\
      \        ( b\n\
      \        , c\n\
      \        )\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \"
    ),
    ( "unattached top level block comment",
      "module X exposing (x)\n\
      \\n\
      \import A\n\
      \\n\
      \\n\
      \\n\
      \{- a -}\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \import A\n\
      \\n\
      \\n\
      \\n\
      \{- a -}\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \"
    ),
    ( "multiline record field in record update",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    { a\n\
      \        | b =\n\
      \            c\n\
      \    }\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    { a\n\
      \        | b =\n\
      \            c\n\
      \    }\n\
      \"
    ),
    ( "multiline record type in type signature",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x :\n\
      \    { a : B\n\
      \    , c : D\n\
      \    }\n\
      \x =\n\
      \    0\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x :\n\
      \    { a : B\n\
      \    , c : D\n\
      \    }\n\
      \x =\n\
      \    0\n\
      \"
    ),
    ( "same line comment after type branch",
      "module X exposing (X)\n\
      \\n\
      \\n\
      \type X\n\
      \    = X -- a\n\
      \",
      "module X exposing (X)\n\
      \\n\
      \\n\
      \type X\n\
      \    = X -- a\n\
      \"
    ),
    ( "block comment before value in multiline record field",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    { a =\n\
      \        {- b -} 0\n\
      \    }\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    { a =\n\
      \        {- b -} 0\n\
      \    }\n\
      \"
    ),
    ( "doc comment on top level bind after type declaration",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \type A\n\
      \    = B\n\
      \\n\
      \\n\
      \{-| c\n\
      \-}\n\
      \x =\n\
      \    d\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \type A\n\
      \    = B\n\
      \\n\
      \\n\
      \{-| c\n\
      \-}\n\
      \x =\n\
      \    d\n\
      \"
    ),
    ( "two line comments inside empty list",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    [-- a\n\
      \     -- b\n\
      \    ]\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    [-- a\n\
      \     -- b\n\
      \    ]\n\
      \"
    ),
    ( "multiline list concatenation",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    a\n\
      \        ++ [ b\n\
      \           ]\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    a\n\
      \        ++ [ b\n\
      \           ]\n\
      \"
    ),
    ( "line comment in list in infix",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    a\n\
      \        ++ [ b\n\
      \\n\
      \           -- c\n\
      \           , d\n\
      \           ]\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    a\n\
      \        ++ [ b\n\
      \\n\
      \           -- c\n\
      \           , d\n\
      \           ]\n\
      \"
    ),
    ( "multiline function type",
      "module X exposing (X)\n\
      \\n\
      \\n\
      \type alias X =\n\
      \    A\n\
      \    -> B\n\
      \",
      "module X exposing (X)\n\
      \\n\
      \\n\
      \type alias X =\n\
      \    A\n\
      \    -> B\n\
      \"
    ),
    ( "block comment after right pizza",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    a\n\
      \        |> {- b -} c\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    a\n\
      \        |> {- b -} c\n\
      \"
    ),
    ( "block comment after expression in infix",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    (a {- b -})\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    (a {- b -})\n\
      \"
    ),
    ( "block comment before expression with both in parentheses",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    ({- a -} b)\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    ({- a -} b)\n\
      \"
    ),
    ( "block comment and expression in multiline parentheses",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    ({- a -} b\n\
      \    )\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    ({- a -} b)\n\
      \"
    ),
    ( "block comment and expression in parentheses in infix",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    (a {- b -}) |> c\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    (a {- b -}) |> c\n\
      \"
    ),
    ( "single line type branch with multi line comment",
      "module X exposing (X)\n\
      \\n\
      \\n\
      \type X\n\
      \    = {- a\n\
      \         b\n\
      \      -}\n\
      \      C D\n\
      \",
      "module X exposing (X)\n\
      \\n\
      \\n\
      \type X\n\
      \    = {- a\n\
      \         b\n\
      \      -}\n\
      \      C D\n\
      \"
    ),
    ( "multi line line comment after type branch",
      "module X exposing (X)\n\
      \\n\
      \\n\
      \type X\n\
      \    = A -- b\n\
      \      -- c\n\
      \    | D\n\
      \",
      "module X exposing (X)\n\
      \\n\
      \\n\
      \type X\n\
      \    = A -- b\n\
      \      -- c\n\
      \    | D\n\
      \"
    ),
    ( "multi line line comment after record type field",
      "module X exposing (X)\n\
      \\n\
      \\n\
      \type alias X =\n\
      \    { a : B\n\
      \\n\
      \    -- c\n\
      \    -- d\n\
      \    }\n\
      \",
      "module X exposing (X)\n\
      \\n\
      \\n\
      \type alias X =\n\
      \    { a : B\n\
      \\n\
      \    -- c\n\
      \    -- d\n\
      \    }\n\
      \"
    ),
    ( "multiline record type in multiline record type field",
      "module X exposing (X)\n\
      \\n\
      \\n\
      \type alias X =\n\
      \    { b :\n\
      \        { c : D\n\
      \        }\n\
      \    }\n\
      \",
      "module X exposing (X)\n\
      \\n\
      \\n\
      \type alias X =\n\
      \    { b :\n\
      \        { c : D\n\
      \        }\n\
      \    }\n\
      \"
    ),
    ( "multiline function type in record type field",
      "module X exposing (X)\n\
      \\n\
      \\n\
      \type alias X =\n\
      \    { a :\n\
      \        A\n\
      \        -> B\n\
      \    }\n\
      \",
      "module X exposing (X)\n\
      \\n\
      \\n\
      \type alias X =\n\
      \    { a :\n\
      \        A\n\
      \        -> B\n\
      \    }\n\
      \"
    ),
    ( "multiline module exports with some documented and some on same line",
      "module X exposing\n\
      \    ( A, B\n\
      \    , C, D\n\
      \    )\n\
      \\n\
      \{-|\n\
      \\n\
      \@docs A, B\n\
      \\n\
      \-}\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \",
      "module X exposing\n\
      \    ( A, B\n\
      \    , C, D\n\
      \    )\n\
      \\n\
      \{-|\n\
      \\n\
      \@docs A, B\n\
      \\n\
      \-}\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \"
    ),
    ( "multiline line comments before expression in let in bind",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    let\n\
      \        a =\n\
      \            -- b\n\
      \            -- c\n\
      \            d\n\
      \    in\n\
      \    e\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    let\n\
      \        a =\n\
      \            -- b\n\
      \            -- c\n\
      \            d\n\
      \    in\n\
      \    e\n\
      \"
    ),
    ( "multiline record in function type in function type",
      "module X exposing (X)\n\
      \\n\
      \\n\
      \type alias X =\n\
      \    ({ a : B\n\
      \     , c : D\n\
      \     }\n\
      \     -> E\n\
      \    )\n\
      \    -> F\n\
      \",
      "module X exposing (X)\n\
      \\n\
      \\n\
      \type alias X =\n\
      \    ({ a : B\n\
      \     , c : D\n\
      \     }\n\
      \     -> E\n\
      \    )\n\
      \    -> F\n\
      \"
    ),
    ( "multiline record type in multiline function type",
      "module X exposing (X)\n\
      \\n\
      \\n\
      \type alias X =\n\
      \    A\n\
      \    ->\n\
      \        { b : C\n\
      \        }\n\
      \",
      "module X exposing (X)\n\
      \\n\
      \\n\
      \type alias X =\n\
      \    A\n\
      \    ->\n\
      \        { b : C\n\
      \        }\n\
      \"
    ),
    ( "type following multiline function type in function type",
      "module X exposing (X)\n\
      \\n\
      \\n\
      \type alias X =\n\
      \    A\n\
      \    ->\n\
      \        (B\n\
      \         -> C\n\
      \        )\n\
      \    -> D\n\
      \",
      "module X exposing (X)\n\
      \\n\
      \\n\
      \type alias X =\n\
      \    A\n\
      \    ->\n\
      \        (B\n\
      \         -> C\n\
      \        )\n\
      \    -> D\n\
      \"
    ),
    ( "multiline function type in multiline function type",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x :\n\
      \    A\n\
      \    ->\n\
      \        (B\n\
      \         -> C\n\
      \        )\n\
      \x =\n\
      \    d\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x :\n\
      \    A\n\
      \    ->\n\
      \        (B\n\
      \         -> C\n\
      \        )\n\
      \x =\n\
      \    d\n\
      \"
    ),
    ( "block comment before type branch name",
      "module X exposing (X)\n\
      \\n\
      \\n\
      \type X\n\
      \    = {- a -} B\n\
      \",
      "module X exposing (X)\n\
      \\n\
      \\n\
      \type X\n\
      \    = {- a -} B\n\
      \"
    ),
    ( "expression after multiline block comment in multiline infix",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    a\n\
      \        |. {- b\n\
      \              c\n\
      \           -}\n\
      \           d\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    a\n\
      \        |. {- b\n\
      \              c\n\
      \           -}\n\
      \           d\n\
      \"
    ),
    ( "line comment after name in multiline branch",
      "module X exposing (X)\n\
      \\n\
      \\n\
      \type X\n\
      \    = A\n\
      \        -- b\n\
      \        C\n\
      \",
      "module X exposing (X)\n\
      \\n\
      \\n\
      \type X\n\
      \    = A\n\
      \        -- b\n\
      \        C\n\
      \"
    ),
    ( "line comment between type parameters in multiline custom type",
      "module X exposing (X)\n\
      \\n\
      \\n\
      \type X\n\
      \    = A\n\
      \        B\n\
      \        -- c\n\
      \        D\n\
      \",
      "module X exposing (X)\n\
      \\n\
      \\n\
      \type X\n\
      \    = A\n\
      \        B\n\
      \        -- c\n\
      \        D\n\
      \"
    ),
    ( "empty list in cons pattern",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    case a of\n\
      \        b :: [] ->\n\
      \            c\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    case a of\n\
      \        b :: [] ->\n\
      \            c\n\
      \"
    ),
    ( "at symbol not followed by docs in doc comment @",
      "module X exposing (x)\n\
      \\n\
      \{-| @\n\
      \-}\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \{-| @\n\
      \-}\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \"
    ),
    ( "float literal with negative exponent",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    1.0e-3\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    1.0e-3\n\
      \"
    ),
    ( "float literal with negative exponent in argument",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    a 1.0e-3\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    a 1.0e-3\n\
      \"
    ),
    ( "infix operator after multiline string on same line",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    \"\"\"\n\
      \\"\"\" ++ a\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    \"\"\"\n\
      \\"\"\" ++ a\n\
      \"
    ),
    ( "multiline string followed by two other items in infix",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    \"\"\"\n\
      \\"\"\" ++ a ++ b\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    \"\"\"\n\
      \\"\"\" ++ a ++ b\n\
      \"
    ),
    ( "block comment inside block comment",
      "module X exposing (x)\n\
      \\n\
      \{- {- a -} -}\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \{- {- a -} -}\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \"
    ),
    ( "doc comment inside block comment",
      "module X exposing (x)\n\
      \\n\
      \{- {-| a -} -}\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \{- {-| a -} -}\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \"
    ),
    ( "block comment inside doc comment",
      "module X exposing (x)\n\
      \\n\
      \{-| {- a -}\n\
      \-}\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \{-| {- a -}\n\
      \-}\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \"
    ),
    ( "doc comment inside doc comment",
      "module X exposing (x)\n\
      \\n\
      \{-| {-| a -}\n\
      \-}\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \{-| {-| a -}\n\
      \-}\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \"
    ),
    ( "block comment at the end of a module",
      "module X exposing (x)\n\
      \\n\
      \{--}\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \{--}\n\
      \"
    ),
    ( "block comment at end of module",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \\n\
      \\n\
      \\n\
      \{- -}\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \\n\
      \\n\
      \\n\
      \{- -}\n\
      \"
    ),
    ( "multiline record type in multiline type argument",
      "module X exposing (X)\n\
      \\n\
      \\n\
      \type alias X =\n\
      \    A\n\
      \        { b : C\n\
      \        }\n\
      \",
      "module X exposing (X)\n\
      \\n\
      \\n\
      \type alias X =\n\
      \    A\n\
      \        { b : C\n\
      \        }\n\
      \"
    ),
    ( "doc comment at end of module",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \\n\
      \\n\
      \{-| a\n\
      \-}\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \\n\
      \\n\
      \{-| a\n\
      \-}\n\
      \"
    ),
    ( "line comment after type declaration followed by top-level declaration",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \type A\n\
      \    = B C -- d\n\
      \\n\
      \\n\
      \x =\n\
      \    e\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \type A\n\
      \    = B C -- d\n\
      \\n\
      \\n\
      \x =\n\
      \    e\n\
      \"
    ),
    ( "no module declaration",
      "x =\n\
      \    0\n\
      \",
      "module Main exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \"
    ),
    ( "import expose all in module with no declaration",
      "import A exposing (..)\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \",
      "module Main exposing (x)\n\
      \\n\
      \import A exposing (..)\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \"
    ),
    ( "line comment at start of module with no declaration and an import",
      "-- a\n\
      \\n\
      \import B exposing (..)\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \",
      "-- a\n\
      \\n\
      \\n\
      \module Main exposing (x)\n\
      \\n\
      \import B exposing (..)\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \"
    ),
    ( "two exposing imports in module with no declaration",
      "import A exposing (b)\n\
      \import C exposing (d)\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \",
      "module Main exposing (x)\n\
      \\n\
      \import A exposing (b)\n\
      \import C exposing (d)\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \"
    ),
    ( "module with no declaration and that starts with a newline",
      "\n\
      \x =\n\
      \    0\n\
      \",
      "module Main exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \"
    ),
    ( "record argument to function in module with no declaration",
      "x =\n\
      \    a { b = c }\n\
      \\n\
      \\n\
      \\n\
      \-- d\n\
      \",
      "module Main exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    a { b = c }\n\
      \\n\
      \\n\
      \\n\
      \-- d\n\
      \"
    ),
    ( "custom type declaration in module with no declaration",
      "type A\n\
      \    = B\n\
      \",
      "module Main exposing (A(..))\n\
      \\n\
      \\n\
      \type A\n\
      \    = B\n\
      \"
    ),
    ( "declaration with newline before equals in module with no declaration",
      "x\n\
      \    = 0\n\
      \",
      "module Main exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \"
    ),
    ( "two top level declarations with no module declaration",
      "x =\n\
      \    0\n\
      \\n\
      \\n\
      \a =\n\
      \    0\n\
      \",
      "module Main exposing (a, x)\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \\n\
      \\n\
      \a =\n\
      \    0\n\
      \"
    ),
    ( "trailing newline in module without module declaration",
      "x =\n\
      \    0\n\
      \\n\
      \",
      "module Main exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \"
    ),
    ( "line comment in module export list",
      "module X exposing\n\
      \    ( a\n\
      \      --\n\
      \    )\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \",
      "module X exposing\n\
      \    ( a\n\
      \      --\n\
      \    )\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \"
    ),
    ( "negative variable",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    -a\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    -a\n\
      \"
    ),
    ( "line comment on same line as let in bind",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    let\n\
      \        a = b -- c\n\
      \    in\n\
      \    a\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    let\n\
      \        a =\n\
      \            b\n\
      \\n\
      \        -- c\n\
      \    in\n\
      \    a\n\
      \"
    ),
    ( "negative variable in function argument",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    a -b\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    a -b\n\
      \"
    ),
    ( "multiline infix operator after function call in list",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    [ a\n\
      \        b\n\
      \      <|\n\
      \        c\n\
      \    ]\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    [ a\n\
      \        b\n\
      \      <|\n\
      \        c\n\
      \    ]\n\
      \"
    ),
    ( "single line block comment between multiline string and infix",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    \"\"\"\n\
      \\"\"\" {--} ++ \" \"\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    \"\"\"\n\
      \\"\"\"\n\
      \        {--}\n\
      \        ++ \" \"\n\
      \"
    ),
    ( "single line block comment after multiline string and infix",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    \"\"\"\n\
      \\"\"\" ++ {--} \" \"\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    \"\"\"\n\
      \\"\"\"\n\
      \        ++ {--}\n\
      \           \" \"\n\
      \"
    ),
    ( "multiline left pizza infix in multiline left pizza infix",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    a\n\
      \        b\n\
      \    <|\n\
      \        c <|\n\
      \            d\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    a\n\
      \        b\n\
      \    <|\n\
      \        c <|\n\
      \            d\n\
      \"
    ),
    ( "function call in list in function argument",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    a [ b c ]\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    a [ b c ]\n\
      \"
    ),
    ( "section comment following custom type declaration",
      "module X exposing (X)\n\
      \\n\
      \\n\
      \type X\n\
      \    = A\n\
      \\n\
      \\n\
      \\n\
      \-- b\n\
      \",
      "module X exposing (X)\n\
      \\n\
      \\n\
      \type X\n\
      \    = A\n\
      \\n\
      \\n\
      \\n\
      \-- b\n\
      \"
    ),
    ( "multiline record update in tuple",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    ( { a\n\
      \        | b = c\n\
      \      }\n\
      \    , d\n\
      \    )\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    ( { a\n\
      \        | b = c\n\
      \      }\n\
      \    , d\n\
      \    )\n\
      \"
    ),
    ( "if then else in else in list",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    [ if a then\n\
      \        b\n\
      \\n\
      \      else if c then\n\
      \        d\n\
      \\n\
      \      else\n\
      \        e\n\
      \    ]\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    [ if a then\n\
      \        b\n\
      \\n\
      \      else if c then\n\
      \        d\n\
      \\n\
      \      else\n\
      \        e\n\
      \    ]\n\
      \"
    ),
    ( "section comment following type branch declaration with parameters",
      "module X exposing (X)\n\
      \\n\
      \\n\
      \type X\n\
      \    = A B\n\
      \\n\
      \\n\
      \\n\
      \-- c\n\
      \",
      "module X exposing (X)\n\
      \\n\
      \\n\
      \type X\n\
      \    = A B\n\
      \\n\
      \\n\
      \\n\
      \-- c\n\
      \"
    ),
    ( "line comment above subsequent let in bind",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    let\n\
      \        a =\n\
      \            b\n\
      \\n\
      \        -- c\n\
      \        d =\n\
      \            e\n\
      \    in\n\
      \    f\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    let\n\
      \        a =\n\
      \            b\n\
      \\n\
      \        -- c\n\
      \        d =\n\
      \            e\n\
      \    in\n\
      \    f\n\
      \"
    ),
    ( "function call in infix in let in bind unformatted",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \  let\n\
      \    a =\n\
      \      b <| c d\n\
      \  in\n\
      \  a\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    let\n\
      \        a =\n\
      \            b <| c d\n\
      \    in\n\
      \    a\n\
      \"
    ),
    ( "line comment following top level bind inline",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \a =\n\
      \    b -- c\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \a =\n\
      \    b\n\
      \\n\
      \\n\
      \\n\
      \-- c\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \"
    ),
    ( "list function argument in infix in let in bind",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \  let\n\
      \    a =\n\
      \      b <| c\n\
      \        []\n\
      \  in\n\
      \  a\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    let\n\
      \        a =\n\
      \            b <|\n\
      \                c\n\
      \                    []\n\
      \    in\n\
      \    a\n\
      \"
    ),
    ( "type alias declaration with no module declaration",
      "type alias X =\n\
      \    A\n\
      \",
      "module Main exposing (X)\n\
      \\n\
      \\n\
      \type alias X =\n\
      \    A\n\
      \"
    ),
    ( "multiline string in module without declaration",
      "a = \"\"\"\n\
      \b\n\
      \\"\"\"\n\
      \",
      "module Main exposing (a)\n\
      \\n\
      \\n\
      \a =\n\
      \    \"\"\"\n\
      \b\n\
      \\"\"\"\n\
      \"
    ),
    ( "sorting exposed imports",
      "module X exposing (x)\n\
      \\n\
      \import A exposing (c, b)\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \import A exposing (b, c)\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \"
    ),
    ( "multiline list followed by infix on same line",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    [ a\n\
      \    ] ++ b\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    [ a\n\
      \    ]\n\
      \        ++ b\n\
      \"
    ),
    ( "sort module export rows",
      "module X exposing\n\
      \    ( b\n\
      \    , a\n\
      \    )\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \",
      "module X exposing\n\
      \    ( a\n\
      \    , b\n\
      \    )\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \"
    ),
    ( "first two function arguments on same line and third on new line",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    a b c\n\
      \        d\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    a b\n\
      \        c\n\
      \        d\n\
      \"
    ),
    ( "top level line comments at top of module with empty lines between them",
      "module X exposing (x)\n\
      \\n\
      \--\n\
      \\n\
      \--\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \--\n\
      \--\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \"
    ),
    ( "top level line comments with an empty line between them",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \\n\
      \\n\
      \\n\
      \--\n\
      \\n\
      \--\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \\n\
      \\n\
      \\n\
      \--\n\
      \--\n\
      \"
    ),
    ( "single line if then else in single line function call",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    a (if b then c else d)\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    a\n\
      \        (if b then\n\
      \            c\n\
      \\n\
      \         else\n\
      \            d\n\
      \        )\n\
      \"
    ),
    ( "function call with first argument if then else and second plain",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    a (if b then c else d) e\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    a\n\
      \        (if b then\n\
      \            c\n\
      \\n\
      \         else\n\
      \            d\n\
      \        )\n\
      \        e\n\
      \"
    ),
    ( "no break space in simple string literal",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    \" \"\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    \"\\u{00A0}\"\n\
      \"
    ),
    ( "no break space in triple quote string literal",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    \"\"\" \"\"\"\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    \"\"\"\\u{00A0}\"\"\"\n\
      \"
    ),
    ( "no break space in multi line triple quote string literal",
      "module Main exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    \"\"\"\n\
      \\160\n\
      \\"\"\"\n\
      \",
      "module Main exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    \"\"\"\n\
      \\\u{00A0}\n\
      \\"\"\"\n\
      \"
    ),
    ( "no break space in multi character simple string literal",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    \"  \"\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    \" \\u{00A0}\"\n\
      \"
    ),
    ( "unindented body in module documentation",
      "module X exposing (x)\n\
      \\n\
      \{-\n\
      \a\n\
      \-}\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \{-\n\
      \   a\n\
      \-}\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \"
    ),
    ( "left pizza on its own line",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    a\n\
      \    <|\n\
      \        c\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    a <|\n\
      \        c\n\
      \"
    ),
    ( "left pizza after multiline function argument",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    a\n\
      \        [ b\n\
      \        ] <|\n\
      \        c\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    a\n\
      \        [ b\n\
      \        ]\n\
      \    <|\n\
      \        c\n\
      \"
    ),
    ( "unnecessary parentheses around infixed verbatim item",
      "module Main exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    (a)\n\
      \        ++ b\n\
      \",
      "module Main exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    a\n\
      \        ++ b\n\
      \"
    ),
    ( "unnecessary parentheses around infixed function call",
      "module Main exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    (a b)\n\
      \        ++ c\n\
      \",
      "module Main exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    a b\n\
      \        ++ c\n\
      \"
    ),
    ( "unnecessary parentheses around infixed record declaration",
      "module Main exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    ({ a = b })\n\
      \        ++ c\n\
      \",
      "module Main exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    { a = b }\n\
      \        ++ c\n\
      \"
    ),
    ( "unnecessary parentheses around infixed record update",
      "module Main exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    ({ a | b = c })\n\
      \        ++ d\n\
      \",
      "module Main exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    { a | b = c }\n\
      \        ++ d\n\
      \"
    ),
    ( "unnecessary parentheses around infixed number",
      "module Main exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    (1)\n\
      \        + a\n\
      \",
      "module Main exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    1\n\
      \        + a\n\
      \"
    ),
    ( "verbatim in parentheses in function argument",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    a (b)\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    a b\n\
      \"
    ),
    ( "single line if then else in single line record field",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    { a = if b then c else d\n\
      \    }\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    { a =\n\
      \        if b then\n\
      \            c\n\
      \\n\
      \        else\n\
      \            d\n\
      \    }\n\
      \"
    ),
    ( "if then else in second item in infix",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    a + (if b then c else d)\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    a\n\
      \        + (if b then\n\
      \            c\n\
      \\n\
      \           else\n\
      \            d\n\
      \          )\n\
      \"
    ),
    ( "extra newline at end of doc comment",
      "module X exposing (x)\n\
      \\n\
      \{-| a\n\
      \\n\
      \-}\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \{-| a\n\
      \-}\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \"
    ),
    ( "bare anonymous function in infix",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    a |> \\b -> c\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    a |> (\\b -> c)\n\
      \"
    ),
    ( "bare as pattern",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    case a of\n\
      \        C d as e ->\n\
      \            f\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    case a of\n\
      \        (C d) as e ->\n\
      \            f\n\
      \"
    ),
    ( "undocumented module exports on single line",
      "module X exposing\n\
      \    ( a\n\
      \    , b, c\n\
      \    )\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \",
      "module X exposing\n\
      \    ( a\n\
      \    , b\n\
      \    , c\n\
      \    )\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \"
    ),
    ( "multi line module doc comment with trailing newline",
      "module X exposing (x)\n\
      \\n\
      \{-| a\n\
      \\n\
      \b\n\
      \\n\
      \-}\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \{-| a\n\
      \\n\
      \b\n\
      \\n\
      \-}\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \"
    ),
    ( "single line if then else in parentheses",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    (if a then b else c)\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    if a then\n\
      \        b\n\
      \\n\
      \    else\n\
      \        c\n\
      \"
    ),
    ( "duplicate imports",
      "module X exposing (x)\n\
      \\n\
      \import A\n\
      \import A\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \import A\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \"
    ),
    ( "if then else in single line anonymous function body",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    \\a -> if b then c else d\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    \\a ->\n\
      \        if b then\n\
      \            c\n\
      \\n\
      \        else\n\
      \            d\n\
      \"
    ),
    ( "pair of hyphens on first line of module documentation",
      "module X exposing (x)\n\
      \\n\
      \{-| --\n\
      \-}\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \{-| --\n\
      \-}\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \"
    ),
    ( "anonymous function in argument bound to name beginning with type",
      "module X exposing (typea)\n\
      \\n\
      \\n\
      \typea =\n\
      \    a (\\a -> a)\n\
      \",
      "module X exposing (typea)\n\
      \\n\
      \\n\
      \typea =\n\
      \    a (\\a -> a)\n\
      \"
    ),
    ( "callable if then else",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    (if a then\n\
      \        b\n\
      \\n\
      \     else\n\
      \        c\n\
      \    )\n\
      \        d\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    (if a then\n\
      \        b\n\
      \\n\
      \     else\n\
      \        c\n\
      \    )\n\
      \        d\n\
      \"
    ),
    ( "string concatenation in argument to if then else",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    (if a then\n\
      \        b\n\
      \\n\
      \     else\n\
      \        c\n\
      \    )\n\
      \        d\n\
      \        ++ e\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    (if a then\n\
      \        b\n\
      \\n\
      \     else\n\
      \        c\n\
      \    )\n\
      \        d\n\
      \        ++ e\n\
      \"
    ),
    ( "infix in brackets in callable",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    (a >> b) c\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    (a >> b) c\n\
      \"
    ),
    ( "multiline infix expression in callable",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    (a\n\
      \        >> b\n\
      \    )\n\
      \        c\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    (a\n\
      \        >> b\n\
      \    )\n\
      \        c\n\
      \"
    ),
    ( "line comment at end of module following empty lines and block comment",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \\n\
      \\n\
      \{-| -}\n\
      \\n\
      \\n\
      \\n\
      \--\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \\n\
      \\n\
      \{-| -}\n\
      \\n\
      \\n\
      \\n\
      \--\n\
      \"
    ),
    ( "line comments at end of module following empty space and block comment",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \\n\
      \\n\
      \{-| -}\n\
      \\n\
      \\n\
      \\n\
      \--\n\
      \--\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \\n\
      \\n\
      \{-| -}\n\
      \\n\
      \\n\
      \\n\
      \--\n\
      \--\n\
      \"
    ),
    ( "remove multiple spaces in text in block comment",
      "module X exposing (x)\n\
      \\n\
      \{-| a  b\n\
      \-}\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \{-| a b\n\
      \-}\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \"
    ),
    ( "multi line record type in argument in record field",
      "module X exposing (X)\n\
      \\n\
      \\n\
      \type alias X =\n\
      \    { a :\n\
      \        B\n\
      \            { c : D\n\
      \            }\n\
      \    }\n\
      \",
      "module X exposing (X)\n\
      \\n\
      \\n\
      \type alias X =\n\
      \    { a :\n\
      \        B\n\
      \            { c : D\n\
      \            }\n\
      \    }\n\
      \"
    ),
    ( "record type in type parameter in type parameter",
      "module X exposing (X)\n\
      \\n\
      \\n\
      \type X\n\
      \    = A\n\
      \        (B\n\
      \            { c : D\n\
      \            }\n\
      \        )\n\
      \",
      "module X exposing (X)\n\
      \\n\
      \\n\
      \type X\n\
      \    = A\n\
      \        (B\n\
      \            { c : D\n\
      \            }\n\
      \        )\n\
      \"
    ),
    ( "multi line tuple type",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \type alias X =\n\
      \    ( A, B\n\
      \    )\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \type alias X =\n\
      \    ( A\n\
      \    , B\n\
      \    )\n\
      \"
    ),
    ( "multi line record type in tuple type",
      "module X exposing (X)\n\
      \\n\
      \\n\
      \type alias X =\n\
      \    ( A\n\
      \    , { b : C\n\
      \      }\n\
      \    )\n\
      \",
      "module X exposing (X)\n\
      \\n\
      \\n\
      \type alias X =\n\
      \    ( A\n\
      \    , { b : C\n\
      \      }\n\
      \    )\n\
      \"
    ),
    ( "unordered list in module doc comment",
      "module X exposing (x)\n\
      \\n\
      \{-| a\n\
      \\n\
      \  - b\n\
      \  - c\n\
      \\n\
      \-}\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \{-| a\n\
      \\n\
      \  - b\n\
      \  - c\n\
      \\n\
      \-}\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \"
    ),
    ( "line comment in else branch in function argument",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    a\n\
      \        (if b then\n\
      \            c\n\
      \\n\
      \         else\n\
      \            -- d\n\
      \            e\n\
      \        )\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    a\n\
      \        (if b then\n\
      \            c\n\
      \\n\
      \         else\n\
      \            -- d\n\
      \            e\n\
      \        )\n\
      \"
    ),
    ( "multi line comment in else branch in function argument",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    a\n\
      \        (if b then\n\
      \            c\n\
      \\n\
      \         else\n\
      \            --\n\
      \            --\n\
      \            d\n\
      \        )\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    a\n\
      \        (if b then\n\
      \            c\n\
      \\n\
      \         else\n\
      \            --\n\
      \            --\n\
      \            d\n\
      \        )\n\
      \"
    ),
    ( "line comments before argument in list",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    [ a\n\
      \        --\n\
      \        --\n\
      \        b\n\
      \    ]\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    [ a\n\
      \        --\n\
      \        --\n\
      \        b\n\
      \    ]\n\
      \"
    ),
    ( "let in inside else inside list",
      "module Main exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    [ if a then\n\
      \        b\n\
      \\n\
      \      else\n\
      \        let\n\
      \            c =\n\
      \                d\n\
      \        in\n\
      \        e\n\
      \    ]\n\
      \",
      "module Main exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    [ if a then\n\
      \        b\n\
      \\n\
      \      else\n\
      \        let\n\
      \            c =\n\
      \                d\n\
      \        in\n\
      \        e\n\
      \    ]\n\
      \"
    ),
    ( "multiline list in if in else in list",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    [ if a then\n\
      \        b\n\
      \\n\
      \      else if\n\
      \        [ c\n\
      \        ]\n\
      \      then\n\
      \        d\n\
      \\n\
      \      else\n\
      \        e\n\
      \    ]\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    [ if a then\n\
      \        b\n\
      \\n\
      \      else if\n\
      \        [ c\n\
      \        ]\n\
      \      then\n\
      \        d\n\
      \\n\
      \      else\n\
      \        e\n\
      \    ]\n\
      \"
    ),
    ( "unnecessary import alias",
      "module X exposing (x)\n\
      \\n\
      \import A as A\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \import A\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \"
    ),
    ( "multi line comment after record field",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    { a = b\n\
      \\n\
      \    --\n\
      \    --\n\
      \    , c = d\n\
      \    }\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    { a = b\n\
      \\n\
      \    --\n\
      \    --\n\
      \    , c = d\n\
      \    }\n\
      \"
    ),
    ( "line comment following case branch in list",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    [ case a of\n\
      \        B ->\n\
      \            c\n\
      \\n\
      \    --\n\
      \    , e\n\
      \    ]\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    [ case a of\n\
      \        B ->\n\
      \            c\n\
      \\n\
      \    --\n\
      \    , e\n\
      \    ]\n\
      \"
    ),
    ( "two hyphens following new line and text in module documentation",
      "module X exposing (x)\n\
      \\n\
      \{-| a\n\
      \\n\
      \--\n\
      \\n\
      \-}\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \{-| a\n\
      \\n\
      \--\n\
      \\n\
      \-}\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \"
    ),
    ( "at docs following hyphen",
      "module X exposing (b, a)\n\
      \\n\
      \{-|\n\
      \\n\
      \-\n\
      \\n\
      \@docs b, a\n\
      \\n\
      \-}\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \",
      "module X exposing (b, a)\n\
      \\n\
      \{-|\n\
      \\n\
      \@docs b, a\n\
      \\n\
      \-}\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \"
    ),
    ( "at docs in module docs following nested block comment",
      "module X exposing (b, a)\n\
      \\n\
      \{-| {- -}\n\
      \\n\
      \@docs b, a\n\
      \\n\
      \-}\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \",
      "module X exposing (b, a)\n\
      \\n\
      \{-| {- -}\n\
      \\n\
      \@docs b, a\n\
      \\n\
      \-}\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \"
    ),
    ( "at docs following ordinary text",
      "module Element.Region exposing (b, a)\n\
      \\n\
      \{-| a\n\
      \\n\
      \@docs b, a\n\
      \\n\
      \-}\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \",
      "module Element.Region exposing (b, a)\n\
      \\n\
      \{-| a\n\
      \\n\
      \@docs b, a\n\
      \\n\
      \-}\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \"
    ),
    ( "open curly bracket following at docs",
      "module X exposing (b, a)\n\
      \\n\
      \{-|\n\
      \\n\
      \@docs b, a\n\
      \\n\
      \{\n\
      \\n\
      \-}\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \",
      "module X exposing (b, a)\n\
      \\n\
      \{-|\n\
      \\n\
      \@docs b, a\n\
      \\n\
      \{\n\
      \\n\
      \-}\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \"
    ),
    ( "close curly bracket following at docs",
      "module X exposing (b, a)\n\
      \\n\
      \{-|\n\
      \\n\
      \@docs b, a\n\
      \\n\
      \}\n\
      \\n\
      \-}\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \",
      "module X exposing (b, a)\n\
      \\n\
      \{-|\n\
      \\n\
      \@docs b, a\n\
      \\n\
      \}\n\
      \\n\
      \-}\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \"
    ),
    ( "consecutive top level block comments at top of module",
      "module X exposing (x)\n\
      \\n\
      \{- -} {- -}\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \{- -} {- -}\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \"
    ),
    ( "consecutive section block comments",
      "module X exposing (x)\n\
      \\n\
      \import A\n\
      \\n\
      \\n\
      \\n\
      \{- -}\n\
      \{- -}\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \import A\n\
      \\n\
      \\n\
      \\n\
      \{- -}\n\
      \{- -}\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \"
    ),
    ( "ordinary text in doc comment",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \\n\
      \\n\
      \{-| a\n\
      \\n\
      \b\n\
      \\n\
      \-}\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \\n\
      \\n\
      \{-| a\n\
      \\n\
      \b\n\
      \\n\
      \-}\n\
      \"
    ),
    ( "ordinary text following two hyphens in doc comment at end of module",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \\n\
      \\n\
      \{-| --\n\
      \\n\
      \a\n\
      \\n\
      \-}\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \\n\
      \\n\
      \{-| --\n\
      \\n\
      \a\n\
      \\n\
      \-}\n\
      \"
    ),
    ( "nested unordered list",
      "module X exposing (x)\n\
      \\n\
      \{-| a\n\
      \\n\
      \  - b\n\
      \      - c\n\
      \\n\
      \-}\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \{-| a\n\
      \\n\
      \  - b\n\
      \      - c\n\
      \\n\
      \-}\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \"
    ),
    ( "empty line before reduction in unordered list nesting",
      "module X exposing (x)\n\
      \\n\
      \{-|\n\
      \\n\
      \  - b\n\
      \      - c\n\
      \\n\
      \  - f\n\
      \\n\
      \-}\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \{-|\n\
      \\n\
      \  - b\n\
      \      - c\n\
      \\n\
      \  - f\n\
      \\n\
      \-}\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \"
    ),
    ( "bullet point beginning with arrow",
      "module X exposing (x)\n\
      \\n\
      \{-|\n\
      \\n\
      \  - a\n\
      \    -> b\n\
      \\n\
      \-}\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \{-|\n\
      \\n\
      \  - a\n\
      \    -> b\n\
      \\n\
      \-}\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \"
    ),
    ( "multi line string in list",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    [ \"\"\"\n\
      \\"\"\" ]\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    [ \"\"\"\n\
      \\"\"\" ]\n\
      \"
    ),
    ( "multi line string in third function argument",
      "module Main exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    a b c \"\"\"\n\
      \\"\"\"\n\
      \",
      "module Main exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    a b c \"\"\"\n\
      \\"\"\"\n\
      \"
    ),
    ( "multi line list in if branch in function argument",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    a\n\
      \        (if b then\n\
      \            [ c\n\
      \            ]\n\
      \\n\
      \         else\n\
      \            a\n\
      \        )\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    a\n\
      \        (if b then\n\
      \            [ c\n\
      \            ]\n\
      \\n\
      \         else\n\
      \            a\n\
      \        )\n\
      \"
    ),
    ( "double line comment before record value",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    { a =\n\
      \        --\n\
      \        --\n\
      \        b\n\
      \    }\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    { a =\n\
      \        --\n\
      \        --\n\
      \        b\n\
      \    }\n\
      \"
    ),
    ( "triple quoted string in multi line list in function argument",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    a [ \"\"\"\"\"\"\n\
      \        ]\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    a\n\
      \        [ \"\"\"\"\"\"\n\
      \        ]\n\
      \"
    ),
    ( "special forward slash infix operator for URL parsers",
      "module X exposing (x)\n\
      \\n\
      \import Url.Parser exposing ((</>))\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \import Url.Parser exposing ((</>))\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \"
    ),
    ( "record destructuring inside list cons pattern",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    case a of\n\
      \        { b } :: c ->\n\
      \            d\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    case a of\n\
      \        { b } :: c ->\n\
      \            d\n\
      \"
    ),
    ( "line and block comments at start of module with only a type in it",
      "--\n\
      \{-\n\
      \\n\
      \-}\n\
      \\n\
      \\n\
      \module X exposing (X)\n\
      \\n\
      \\n\
      \type X\n\
      \    = Y\n\
      \",
      "--\n\
      \{-\n\
      \\n\
      \-}\n\
      \\n\
      \\n\
      \module X exposing (X)\n\
      \\n\
      \\n\
      \type X\n\
      \    = Y\n\
      \"
    ),
    ( "multiline record type in first of two type arguments",
      "module X exposing (X)\n\
      \\n\
      \\n\
      \type alias X =\n\
      \    A\n\
      \        { b : C\n\
      \        }\n\
      \        D\n\
      \",
      "module X exposing (X)\n\
      \\n\
      \\n\
      \type alias X =\n\
      \    A\n\
      \        { b : C\n\
      \        }\n\
      \        D\n\
      \"
    ),
    ( "Function type in tuple type",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x : ( A -> b, c )\n\
      \x =\n\
      \    0\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x : ( A -> b, c )\n\
      \x =\n\
      \    0\n\
      \"
    ),
    ( "multiline record type in function type in function type",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x :\n\
      \    (A\n\
      \     ->\n\
      \        { b : C\n\
      \        }\n\
      \    )\n\
      \    -> D\n\
      \x =\n\
      \    a\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x :\n\
      \    (A\n\
      \     ->\n\
      \        { b : C\n\
      \        }\n\
      \    )\n\
      \    -> D\n\
      \x =\n\
      \    a\n\
      \"
    ),
    ( "line comment in type signature",
      "module Y exposing (y)\n\
      \\n\
      \\n\
      \y :\n\
      \    --\n\
      \    A\n\
      \y =\n\
      \    0\n\
      \",
      "module Y exposing (y)\n\
      \\n\
      \\n\
      \y :\n\
      \    --\n\
      \    A\n\
      \y =\n\
      \    0\n\
      \"
    ),
    ( "superfluous parentheses around type in type signature",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x : (A)\n\
      \x =\n\
      \    0\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x : A\n\
      \x =\n\
      \    0\n\
      \"
    ),
    ( "superfluous parentheses around type with arguments",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x : (A B)\n\
      \x =\n\
      \    0\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x : A B\n\
      \x =\n\
      \    0\n\
      \"
    ),
    ( "end of documentation comment on same line as contents",
      "module X exposing (x)\n\
      \\n\
      \{-|a-}\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \{-| a\n\
      \-}\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \"
    ),
    ( "unnecessary parentheses around pattern in case expression",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    case a of\n\
      \        (B) ->\n\
      \            c\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    case a of\n\
      \        B ->\n\
      \            c\n\
      \"
    ),
    ( "empty doc comment on function",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \a =\n\
      \    0\n\
      \\n\
      \\n\
      \{-|\n\
      \-}\n\
      \x =\n\
      \    0\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \a =\n\
      \    0\n\
      \\n\
      \\n\
      \{-| -}\n\
      \x =\n\
      \    0\n\
      \"
    ),
    ( "empty multi-line doc comment",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \a =\n\
      \    0\n\
      \\n\
      \\n\
      \{-| \n\
      \-}\n\
      \x =\n\
      \    0\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \a =\n\
      \    0\n\
      \\n\
      \\n\
      \{-| -}\n\
      \x =\n\
      \    0\n\
      \"
    ),
    ( "trailing space in doc comment",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \a =\n\
      \    0\n\
      \\n\
      \\n\
      \{-| b \n\
      \-}\n\
      \x =\n\
      \    0\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \a =\n\
      \    0\n\
      \\n\
      \\n\
      \{-| b\n\
      \-}\n\
      \x =\n\
      \    0\n\
      \"
    ),
    ( "unnecessary parentheses around type argument",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x : A (a)\n\
      \x =\n\
      \    0\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x : A a\n\
      \x =\n\
      \    0\n\
      \"
    ),
    ( "compound record lookup with equality operator",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    (a b).c == d\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    (a b).c == d\n\
      \"
    ),
    ( "multiple line comments on right of right pizza",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    a\n\
      \        |> --\n\
      \           --\n\
      \           b\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    a\n\
      \        |> --\n\
      \           --\n\
      \           b\n\
      \"
    ),
    ( "single line comment after right pizza",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    a\n\
      \        |> --\n\
      \           b\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    a\n\
      \        |> --\n\
      \           b\n\
      \"
    ),
    ( "line comment at start of line inside case expression",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    case a of\n\
      \        b ->\n\
      \            c\n\
      \\n\
      \--\n\
      \        d ->\n\
      \            e\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    case a of\n\
      \        b ->\n\
      \            c\n\
      \\n\
      \        --\n\
      \        d ->\n\
      \            e\n\
      \"
    ),
    ( "two spaces between numbers and list items in comments",
      "module X exposing (x)\n\
      \\n\
      \{-| a\n\
      \\n\
      \1.  b\n\
      \2.  c\n\
      \\n\
      \-}\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \{-| a\n\
      \\n\
      \1.  b\n\
      \2.  c\n\
      \\n\
      \-}\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \"
    ),
    ( "numbered list with parentheses",
      "module X exposing (x)\n\
      \\n\
      \{-| a\n\
      \\n\
      \1)  b\n\
      \2)  c\n\
      \\n\
      \-}\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \{-| a\n\
      \\n\
      \1.  b\n\
      \2.  c\n\
      \\n\
      \-}\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \"
    ),
    ( "wrongly numbered list items in comment",
      "module X exposing (x)\n\
      \\n\
      \{-| a\n\
      \\n\
      \1.  b\n\
      \1.  c\n\
      \\n\
      \-}\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \{-| a\n\
      \\n\
      \1.  b\n\
      \2.  c\n\
      \\n\
      \-}\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \"
    ),
    ( "numbered list item with two digit number",
      "module X exposing (x)\n\
      \\n\
      \{-| a\n\
      \\n\
      \1.  b\n\
      \2.  b\n\
      \3.  b\n\
      \4.  b\n\
      \5.  b\n\
      \6.  b\n\
      \7.  b\n\
      \8.  b\n\
      \9.  b\n\
      \10. b\n\
      \\n\
      \-}\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \{-| a\n\
      \\n\
      \1.  b\n\
      \2.  b\n\
      \3.  b\n\
      \4.  b\n\
      \5.  b\n\
      \6.  b\n\
      \7.  b\n\
      \8.  b\n\
      \9.  b\n\
      \10. b\n\
      \\n\
      \-}\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \"
    ),
    ( "two spaces before hyphen in numbered list item",
      "module X exposing (x)\n\
      \\n\
      \{-| a\n\
      \\n\
      \1.  b  - c\n\
      \2.  d\n\
      \\n\
      \-}\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \{-| a\n\
      \\n\
      \1.  b - c\n\
      \2.  d\n\
      \\n\
      \-}\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \"
    ),
    ( "missing trailing newline in doc comment",
      "module X exposing (x)\n\
      \\n\
      \{-| a\n\
      \\n\
      \b\n\
      \-}\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \{-| a\n\
      \\n\
      \b\n\
      \\n\
      \-}\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \"
    ),
    ( "case expression without parentheses in infix",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    a\n\
      \        :: case b of\n\
      \            C ->\n\
      \         d\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    a\n\
      \        :: (case b of\n\
      \                C ->\n\
      \                    d\n\
      \           )\n\
      \"
    ),
    ( "case expression with no parentheses and bad indent",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    a\n\
      \    ::\n\
      \    case b of\n\
      \      C ->\n\
      \        d\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    a\n\
      \        :: (case b of\n\
      \                C ->\n\
      \                    d\n\
      \           )\n\
      \"
    ),
    ( "multi line list after infix",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \  a\n\
      \  ++\n\
      \  [ b,\n\
      \    c\n\
      \  ]\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    a\n\
      \        ++ [ b\n\
      \           , c\n\
      \           ]\n\
      \"
    ),
    ( "lowercase characters in simple string unicode literal",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    \"\\u{001b}\"\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    \"\\u{001B}\"\n\
      \"
    ),
    ( "lowercase characters in char unicode literal",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    '\\u{001b}'\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    '\\u{001B}'\n\
      \"
    ),
    ( "multi line list in infix in function argument",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    a\n\
      \        ([ b\n\
      \         ]\n\
      \            ++ c\n\
      \        )\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    a\n\
      \        ([ b\n\
      \         ]\n\
      \            ++ c\n\
      \        )\n\
      \"
    ),
    ( "doc comment followed by line comment",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \a =\n\
      \    0\n\
      \\n\
      \\n\
      \{-|\n\
      \-} --\n\
      \x =\n\
      \    0\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \a =\n\
      \    0\n\
      \\n\
      \\n\
      \{-| -}\n\
      \\n\
      \\n\
      \\n\
      \--\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \"
    ),
    ( "line comment after then expression",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    if a then\n\
      \        b\n\
      \        --\n\
      \\n\
      \    else\n\
      \        c\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    if a then\n\
      \        b\n\
      \        --\n\
      \\n\
      \    else\n\
      \        c\n\
      \"
    ),
    ( "line comment after if expression",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    if\n\
      \        a\n\
      \        --\n\
      \    then\n\
      \        b\n\
      \\n\
      \    else\n\
      \        c\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    if\n\
      \        a\n\
      \        --\n\
      \    then\n\
      \        b\n\
      \\n\
      \    else\n\
      \        c\n\
      \"
    ),
    ( "line comment before if expression",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    if\n\
      \        --\n\
      \        a\n\
      \    then\n\
      \        b\n\
      \\n\
      \    else\n\
      \        c\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    if\n\
      \        --\n\
      \        a\n\
      \    then\n\
      \        b\n\
      \\n\
      \    else\n\
      \        c\n\
      \"
    ),
    ( "line break after module name",
      "module X\n\
      \  exposing (x)\n\
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
    ),
    ( "block comment after module keyword",
      "module\n\
      \    {--}\n\
      \    X\n\
      \    exposing\n\
      \    (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \",
      "module\n\
      \    {--}\n\
      \    X\n\
      \    exposing\n\
      \    (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \"
    ),
    ( "comment after module name",
      "module X\n\
      \    {--}\n\
      \    exposing\n\
      \    (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \",
      "module X\n\
      \    {--}\n\
      \    exposing\n\
      \    (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \"
    ),
    ( "comment after module exposing keyword",
      "module X\n\
      \    exposing\n\
      \    {--}\n\
      \    (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \",
      "module X\n\
      \    exposing\n\
      \    {--}\n\
      \    (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \"
    ),
    ( "only one empty lines before header in comment",
      "module X exposing (x)\n\
      \\n\
      \{-|\n\
      \\n\
      \#\n\
      \\n\
      \-}\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \{-|\n\
      \\n\
      \\n\
      \#\n\
      \\n\
      \-}\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \"
    ),
    ( "no empty lines before header in comment",
      "module X exposing (x)\n\
      \\n\
      \{-|\n\
      \#\n\
      \\n\
      \-}\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \{-|\n\
      \\n\
      \\n\
      \#\n\
      \\n\
      \-}\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \"
    ),
    ( "no new line following header in doc comment",
      "module X exposing (x)\n\
      \\n\
      \{-|\n\
      \\n\
      \\n\
      \#\n\
      \-}\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \{-|\n\
      \\n\
      \\n\
      \#\n\
      \\n\
      \-}\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \"
    ),
    ( "second level header comment",
      "module X exposing (x)\n\
      \\n\
      \{-|\n\
      \\n\
      \\n\
      \##\n\
      \\n\
      \-}\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \{-|\n\
      \\n\
      \\n\
      \##\n\
      \\n\
      \-}\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \"
    ),
    ( "header comment followed by at doc comment",
      "module X exposing (x)\n\
      \\n\
      \{-|\n\
      \\n\
      \@docs x\n\
      \\n\
      \\n\
      \#\n\
      \\n\
      \-}\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \{-|\n\
      \\n\
      \@docs x\n\
      \\n\
      \\n\
      \#\n\
      \\n\
      \-}\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \"
    ),
    ( "doc section comment following text",
      "module X exposing (x)\n\
      \\n\
      \{-|\n\
      \\n\
      \a\n\
      \\n\
      \\n\
      \#\n\
      \\n\
      \-}\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \{-| a\n\
      \\n\
      \\n\
      \#\n\
      \\n\
      \-}\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \"
    ),
    ( "leading space on ordinary text line in doc comment",
      "module X exposing (x)\n\
      \\n\
      \{-|\n\
      \\n\
      \ a\n\
      \\n\
      \-}\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \{-| a\n\
      \-}\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \"
    ),
    ( "newlines before unordered list at start of doc comment",
      "module X exposing (x)\n\
      \\n\
      \{-|\n\
      \  - a\n\
      \\n\
      \-}\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \{-|\n\
      \\n\
      \  - a\n\
      \\n\
      \-}\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \"
    ),
    ( "doc comment containing single numbered list item",
      "module X exposing (x)\n\
      \\n\
      \{-|\n\
      \\n\
      \1.  a\n\
      \\n\
      \-}\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \{-|\n\
      \\n\
      \1.  a\n\
      \\n\
      \-}\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \"
    ),
    ( "line of text in doc comment beginning with non list numeral",
      "module X exposing (x)\n\
      \\n\
      \{-|\n\
      \\n\
      \1 a\n\
      \\n\
      \-}\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \{-| 1 a\n\
      \-}\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \"
    ),
    ( "single numbered list item without enough trailing whitespace",
      "module X exposing (x)\n\
      \\n\
      \{-|\n\
      \\n\
      \1. a\n\
      \-}\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \{-|\n\
      \\n\
      \1.  a\n\
      \\n\
      \-}\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \"
    ),
    ( "doc comment heading following at docs",
      "module X exposing (x)\n\
      \\n\
      \{-|\n\
      \\n\
      \@docs x\n\
      \\n\
      \#\n\
      \\n\
      \-}\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \{-|\n\
      \\n\
      \@docs x\n\
      \\n\
      \\n\
      \#\n\
      \\n\
      \-}\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \"
    ),
    ( "two newlines between paragraphs in doc comment",
      "module X exposing (x)\n\
      \\n\
      \{-| a\n\
      \\n\
      \\n\
      \b\n\
      \\n\
      \-}\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \{-| a\n\
      \\n\
      \b\n\
      \\n\
      \-}\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \"
    ),
    ( "extra newline after code block in doc comment on module",
      "module X exposing (x)\n\
      \\n\
      \{-|\n\
      \\n\
      \    a\n\
      \\n\
      \\n\
      \-}\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \{-|\n\
      \\n\
      \    a\n\
      \\n\
      \-}\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \"
    ),
    ( "extra newline after code block in doc comment on function",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \a =\n\
      \    0\n\
      \\n\
      \\n\
      \{-|\n\
      \\n\
      \    b\n\
      \\n\
      \\n\
      \-}\n\
      \x =\n\
      \    0\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \a =\n\
      \    0\n\
      \\n\
      \\n\
      \{-|\n\
      \\n\
      \    b\n\
      \\n\
      \-}\n\
      \x =\n\
      \    0\n\
      \"
    ),
    ( "short indent in Elm code in block comment",
      "module X exposing (x)\n\
      \\n\
      \{-|\n\
      \\n\
      \    x =\n\
      \       0\n\
      \\n\
      \-}\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \{-|\n\
      \\n\
      \    x =\n\
      \        0\n\
      \\n\
      \-}\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \"
    ),
    ( "missing newline after code block in doc comment",
      "module X exposing (x)\n\
      \\n\
      \{-|\n\
      \\n\
      \    a =\n\
      \        0\n\
      \-}\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \{-|\n\
      \\n\
      \    a =\n\
      \        0\n\
      \\n\
      \-}\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \"
    ),
    ( "single empty line between top level binds in doc comment",
      "module X exposing (x)\n\
      \\n\
      \{-|\n\
      \\n\
      \    a =\n\
      \        0\n\
      \\n\
      \    b =\n\
      \        0\n\
      \\n\
      \-}\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \{-|\n\
      \\n\
      \    a =\n\
      \        0\n\
      \\n\
      \    b =\n\
      \        0\n\
      \\n\
      \-}\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \"
    ),
    ( "multiline string inside Elm code in doc comment",
      "module X exposing (x)\n\
      \\n\
      \{-|\n\
      \\n\
      \    a =\n\
      \        \"\"\"\n\
      \\n\
      \        \"\"\"\n\
      \\n\
      \-}\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \{-|\n\
      \\n\
      \    a =\n\
      \        \"\"\"\n\
      \\n\
      \        \"\"\"\n\
      \\n\
      \-}\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \"
    ),
    ( "module doc contining only a bulleted list",
      "module X exposing (x)\n\
      \\n\
      \{-|\n\
      \\n\
      \- a\n\
      \- b\n\
      \\n\
      \-}\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \{-|\n\
      \\n\
      \  - a\n\
      \  - b\n\
      \\n\
      \-}\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \"
    ),
    ( "unordered list in comment followed by code block",
      "module X exposing (x)\n\
      \\n\
      \{-|\n\
      \\n\
      \  - a\n\
      \  - b\n\
      \\n\
      \\n\
      \    c =\n\
      \        0\n\
      \\n\
      \-}\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \{-|\n\
      \\n\
      \  - a\n\
      \  - b\n\
      \\n\
      \```\n\
      \c =\n\
      \    0\n\
      \```\n\
      \\n\
      \-}\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \"
    ),
    ( "number ordered list in comment followed by code block",
      "module X exposing (x)\n\
      \\n\
      \{-|\n\
      \\n\
      \1.  a\n\
      \2.  b\n\
      \\n\
      \\n\
      \    c =\n\
      \        0\n\
      \\n\
      \-}\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \{-|\n\
      \\n\
      \1.  a\n\
      \2.  b\n\
      \\n\
      \```\n\
      \c =\n\
      \    0\n\
      \```\n\
      \\n\
      \-}\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \"
    ),
    ( "multi line ordered list item in doc comment",
      "module X exposing (x)\n\
      \\n\
      \{-|\n\
      \\n\
      \1.  a\n\
      \2.  b\n\
      \    c\n\
      \\n\
      \-}\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \{-|\n\
      \\n\
      \1.  a\n\
      \2.  b\n\
      \    c\n\
      \\n\
      \-}\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \"
    ),
    ( "multi line unordered list item",
      "module X exposing (x)\n\
      \\n\
      \{-|\n\
      \\n\
      \-  a\n\
      \-  b\n\
      \   c\n\
      \\n\
      \-}\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \{-|\n\
      \\n\
      \  - a\n\
      \  - b\n\
      \    c\n\
      \\n\
      \-}\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \"
    ),
    ( "nested unordered list unformatted",
      "module X exposing (x)\n\
      \\n\
      \{-|\n\
      \\n\
      \  - a\n\
      \     - b\n\
      \\n\
      \-}\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \{-|\n\
      \\n\
      \  - a\n\
      \      - b\n\
      \\n\
      \-}\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \"
    ),
    ( "nested numbered list in doc comment",
      "module X exposing (x)\n\
      \\n\
      \{-|\n\
      \\n\
      \1.  a\n\
      \    1.  b\n\
      \\n\
      \-}\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \{-|\n\
      \\n\
      \1.  a\n\
      \    1.  b\n\
      \\n\
      \-}\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \"
    ),
    ( "text following code block following numbered list",
      "module X exposing (x)\n\
      \\n\
      \{-|\n\
      \\n\
      \1.  a\n\
      \2.  b\n\
      \\n\
      \\n\
      \    c =\n\
      \        0\n\
      \\n\
      \\n\
      \d\n\
      \\n\
      \-}\n\
      \\n\
      \x =\n\
      \    0\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \{-|\n\
      \\n\
      \1.  a\n\
      \2.  b\n\
      \\n\
      \```\n\
      \c =\n\
      \    0\n\
      \```\n\
      \\n\
      \d\n\
      \\n\
      \-}\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \"
    ),
    ( "consecutive headings in doc comment",
      "module X exposing (x)\n\
      \\n\
      \{-|\n\
      \\n\
      \\n\
      \#\n\
      \\n\
      \\n\
      \#\n\
      \\n\
      \-}\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \{-|\n\
      \\n\
      \\n\
      \#\n\
      \\n\
      \\n\
      \#\n\
      \\n\
      \-}\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \"
    ),
    ( "trailing space on header in doc comment",
      "module X exposing (x)\n\
      \\n\
      \{-|\n\
      \\n\
      \\n\
      \# \n\
      \\n\
      \-}\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \{-|\n\
      \\n\
      \\n\
      \#\n\
      \\n\
      \-}\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \"
    ),
    ( "documentation text beginning with a hash",
      "module X exposing (x)\n\
      \\n\
      \{-| #a\n\
      \-}\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \{-| #a\n\
      \-}\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \"
    ),
    ( "multiple doc rows beginning with a hash",
      "module X exposing (x)\n\
      \\n\
      \{-| #a\n\
      \#b\n\
      \-}\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \{-| #a\n\
      \#b\n\
      \-}\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \"
    ),
    ( "document line beginning with a space",
      "module X exposing (x)\n\
      \\n\
      \{-| a\n\
      \ b\n\
      \-}\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \{-| a\n\
      \b\n\
      \-}\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \"
    ),
    ( "doc comment containing only a non-empty header",
      "module X exposing (x)\n\
      \\n\
      \{-|\n\
      \\n\
      \\n\
      \# a\n\
      \\n\
      \-}\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \{-|\n\
      \\n\
      \\n\
      \# a\n\
      \\n\
      \-}\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \"
    ),
    ( "second level header followed immediately by text line",
      "module X exposing (x)\n\
      \\n\
      \{-|\n\
      \\n\
      \\n\
      \## a\n\
      \b\n\
      \\n\
      \-}\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \{-|\n\
      \\n\
      \\n\
      \## a\n\
      \\n\
      \b\n\
      \\n\
      \-}\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \"
    ),
    ( "three headers in doc comment with at docs after first one",
      "module X exposing (x)\n\
      \\n\
      \{-|\n\
      \\n\
      \\n\
      \# a\n\
      \\n\
      \@docs x\n\
      \\n\
      \\n\
      \# b\n\
      \\n\
      \\n\
      \# c\n\
      \\n\
      \-}\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \{-|\n\
      \\n\
      \\n\
      \# a\n\
      \\n\
      \@docs x\n\
      \\n\
      \\n\
      \# b\n\
      \\n\
      \\n\
      \# c\n\
      \\n\
      \-}\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \"
    ),
    ( "two empty lines after at docs row",
      "module X exposing (x)\n\
      \\n\
      \{-|\n\
      \\n\
      \@docs x\n\
      \\n\
      \\n\
      \-}\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \{-|\n\
      \\n\
      \@docs x\n\
      \\n\
      \-}\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \"
    ),
    ( "extra trailing newline after text after code block after text",
      "module X exposing (x)\n\
      \\n\
      \{-| a\n\
      \\n\
      \    x =\n\
      \        0\n\
      \\n\
      \b\n\
      \\n\
      \\n\
      \-}\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \{-| a\n\
      \\n\
      \    x =\n\
      \        0\n\
      \\n\
      \b\n\
      \\n\
      \-}\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \"
    ),
    ( "three items in unordered list",
      "module X exposing (x)\n\
      \\n\
      \{-|\n\
      \\n\
      \  - a\n\
      \  - b\n\
      \  - c\n\
      \\n\
      \-}\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \{-|\n\
      \\n\
      \  - a\n\
      \  - b\n\
      \  - c\n\
      \\n\
      \-}\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \"
    ),
    ( "two empty lines after text before code block in doc comment",
      "module X exposing (x)\n\
      \\n\
      \{-| a\n\
      \\n\
      \\n\
      \    x =\n\
      \        0\n\
      \\n\
      \-}\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \{-| a\n\
      \\n\
      \    x =\n\
      \        0\n\
      \\n\
      \-}\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \"
    ),
    ( "newline before line comment in code block in doc comment",
      "module X exposing (x)\n\
      \\n\
      \{-|\n\
      \\n\
      \    x =\n\
      \\n\
      \        --\n\
      \        0\n\
      \\n\
      \-}\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \{-|\n\
      \\n\
      \    x =\n\
      \        --\n\
      \        0\n\
      \\n\
      \-}\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \"
    ),
    ( "numbered list following text with no empty line",
      "module X exposing (x)\n\
      \\n\
      \{-| b\n\
      \1.  c\n\
      \2.  d\n\
      \\n\
      \-}\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \{-| b\n\
      \\n\
      \1.  c\n\
      \2.  d\n\
      \\n\
      \-}\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \"
    ),
    ( "code block in doc comment with space before end doc comment",
      "module X exposing (x)\n\
      \\n\
      \{-|\n\
      \\n\
      \    x =\n\
      \       0\n\
      \\n\
      \ -}\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \{-|\n\
      \\n\
      \    x =\n\
      \        0\n\
      \\n\
      \-}\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \"
    ),
    ( "line comment following expression in code block in doc comment",
      "module X exposing (x)\n\
      \\n\
      \{-|\n\
      \\n\
      \\n\
      \    a =\n\
      \        0 --\n\
      \\n\
      \-}\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \{-|\n\
      \\n\
      \\n\
      \    a =\n\
      \        0\n\
      \\n\
      \    --\n\
      \\n\
      \-}\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \"
    ),
    ( "code block in doc comment containing block and line comments",
      "module X exposing (x)\n\
      \\n\
      \{-|\n\
      \\n\
      \    {- a -}\n\
      \    --\n\
      \\n\
      \\n\
      \\n\
      \-}\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \{-|\n\
      \\n\
      \    {- a -}\n\
      \    --\n\
      \\n\
      \\n\
      \\n\
      \-}\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \"
    ),
    ( "no empty line between multi line numbered list items",
      "module X exposing (x)\n\
      \\n\
      \{-|\n\
      \\n\
      \1.  a\n\
      \2.  b\n\
      \\n\
      \    c\n\
      \\n\
      \-}\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \{-|\n\
      \\n\
      \1.  a\n\
      \\n\
      \2.  b\n\
      \\n\
      \    c\n\
      \\n\
      \-}\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \"
    ),
    ( "numbered list with empty line between the items",
      "module X exposing (x)\n\
      \\n\
      \{-|\n\
      \\n\
      \1.  a\n\
      \\n\
      \1.  b\n\
      \\n\
      \-}\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \{-|\n\
      \\n\
      \1.  a\n\
      \\n\
      \2.  b\n\
      \\n\
      \-}\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \"
    ),
    ( "unordered list with an empty line in it",
      "module X exposing (x)\n\
      \\n\
      \{-|\n\
      \\n\
      \  - a\n\
      \  - b\n\
      \\n\
      \  - c\n\
      \\n\
      \-}\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \{-|\n\
      \\n\
      \  - a\n\
      \\n\
      \  - b\n\
      \\n\
      \  - c\n\
      \\n\
      \-}\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \"
    ),
    ( "numbered list followed by plain text row in doc comment",
      "module X exposing (x)\n\
      \\n\
      \{-|\n\
      \\n\
      \1.  a\n\
      \2.  b\n\
      \\n\
      \c\n\
      \\n\
      \-}\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \{-|\n\
      \\n\
      \1.  a\n\
      \2.  b\n\
      \\n\
      \c\n\
      \\n\
      \-}\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \"
    ),
    ( "non elm code block following numbered list item",
      "module X exposing (x)\n\
      \\n\
      \{-|\n\
      \\n\
      \1.  a\n\
      \\n\
      \2.  b\n\
      \\n\
      \    c\n\
      \    d\n\
      \\n\
      \-}\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \{-|\n\
      \\n\
      \1.  a\n\
      \\n\
      \2.  b\n\
      \\n\
      \    c\n\
      \    d\n\
      \\n\
      \-}\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \"
    ),
    ( "code block following numbered list after two empty lines",
      "module X exposing (x)\n\
      \\n\
      \{-|\n\
      \\n\
      \1.  a\n\
      \2.  b\n\
      \\n\
      \\n\
      \    c =\n\
      \        d\n\
      \-}\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \{-|\n\
      \\n\
      \1.  a\n\
      \2.  b\n\
      \\n\
      \```\n\
      \c =\n\
      \    d\n\
      \```\n\
      \\n\
      \-}\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \"
    ),
    ( "non elm code block after two empty lines after numbered list",
      "module X exposing (x)\n\
      \\n\
      \{-|\n\
      \\n\
      \1.  a\n\
      \2.  b\n\
      \\n\
      \\n\
      \    c\n\
      \\n\
      \-}\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \{-|\n\
      \\n\
      \1.  a\n\
      \2.  b\n\
      \\n\
      \```\n\
      \c\n\
      \```\n\
      \\n\
      \-}\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \"
    ),
    ( "code block with line comment following text",
      "module X exposing (x)\n\
      \\n\
      \{-| a\n\
      \\n\
      \\n\
      \    b =\n\
      \        0\n\
      \\n\
      \    --\n\
      \\n\
      \-}\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \{-| a\n\
      \\n\
      \\n\
      \    b =\n\
      \        0\n\
      \\n\
      \    --\n\
      \\n\
      \-}\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \"
    ),
    ( "line comment between declarations in code block in doc comment",
      "module X exposing (x)\n\
      \\n\
      \{-|\n\
      \\n\
      \    a =\n\
      \        0\n\
      \\n\
      \    --\n\
      \    b =\n\
      \        0\n\
      \\n\
      \-}\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \{-|\n\
      \\n\
      \    a =\n\
      \        0\n\
      \\n\
      \    --\n\
      \    b =\n\
      \        0\n\
      \\n\
      \-}\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \"
    ),
    ( "unordered list followed by text in doc comment",
      "module X exposing (x)\n\
      \\n\
      \{-|\n\
      \\n\
      \  - a\n\
      \  - b\n\
      \\n\
      \c\n\
      \\n\
      \-}\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \{-|\n\
      \\n\
      \  - a\n\
      \  - b\n\
      \\n\
      \c\n\
      \\n\
      \-}\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \"
    ),
    ( "doc comment containing just an asterisk",
      "module X exposing (x)\n\
      \\n\
      \{-| *\n\
      \-}\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \{-| -}\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \"
    ),
    ( "only two asterisks in doc comment",
      "module X exposing (x)\n\
      \\n\
      \{-| **\n\
      \-}\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \{-| \\*\\*\n\
      \-}\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \"
    ),
    ( "line comment in code block after two empty lines after list",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \{-|\n\
      \\n\
      \1.  a\n\
      \2.  b\n\
      \\n\
      \\n\
      \    c =\n\
      \        0\n\
      \    --\n\
      \\n\
      \\n\
      \-}\n\
      \x =\n\
      \    0\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \{-|\n\
      \\n\
      \1.  a\n\
      \2.  b\n\
      \\n\
      \```\n\
      \\n\
      \c =\n\
      \    0\n\
      \\n\
      \--\n\
      \```\n\
      \\n\
      \-}\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \"
    ),
    ( "code block in backticks following numbered list in doc comment",
      "module X exposing (x)\n\
      \\n\
      \{-|\n\
      \\n\
      \1.  a\n\
      \2.  b\n\
      \\n\
      \```\n\
      \c =\n\
      \    0\n\
      \```\n\
      \\n\
      \-}\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \{-|\n\
      \\n\
      \1.  a\n\
      \2.  b\n\
      \\n\
      \```\n\
      \c =\n\
      \    0\n\
      \```\n\
      \\n\
      \-}\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \"
    ),
    ( "no leading empty line in code block with trailing line comment",
      "module X exposing (x)\n\
      \\n\
      \{-|\n\
      \\n\
      \1.  a\n\
      \2.  b\n\
      \\n\
      \```\n\
      \c =\n\
      \    0\n\
      \\n\
      \--\n\
      \```\n\
      \\n\
      \-}\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \{-|\n\
      \\n\
      \1.  a\n\
      \2.  b\n\
      \\n\
      \```\n\
      \\n\
      \c =\n\
      \    0\n\
      \\n\
      \--\n\
      \```\n\
      \\n\
      \-}\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \"
    ),
    ( "line comment followed by block comment followed by line comment",
      "module X exposing (x)\n\
      \\n\
      \--\n\
      \\n\
      \\n\
      \{-| -}\n\
      \\n\
      \\n\
      \\n\
      \--\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \{-| -}\n\
      \\n\
      \--\n\
      \--\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \"
    ),
    ( "asterisks in independent doc comment",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \a =\n\
      \    0\n\
      \\n\
      \\n\
      \{-| **\n\
      \-}\n\
      \\n\
      \\n\
      \\n\
      \--\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \a =\n\
      \    0\n\
      \\n\
      \\n\
      \{-| \\*\\*\n\
      \-}\n\
      \\n\
      \\n\
      \\n\
      \--\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \"
    ),
    ( "doc comment containing asterisks at end of module",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \\n\
      \\n\
      \{-| **\n\
      \-}\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \\n\
      \\n\
      \{-| \\*\\*\n\
      \-}\n\
      \"
    ),
    ( "multiline independent block comment containing single line",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \\n\
      \\n\
      \\n\
      \{- a\n\
      \-}\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \\n\
      \\n\
      \\n\
      \{- a -}\n\
      \"
    ),
    ( "no empty line before at docs",
      "module X exposing (x)\n\
      \\n\
      \{-|\n\
      \@docs x\n\
      \\n\
      \-}\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \{-|\n\
      \\n\
      \@docs x\n\
      \\n\
      \-}\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \"
    ),
    ( "empty unordered list item",
      "module X exposing (x)\n\
      \\n\
      \{-|\n\
      \\n\
      \  - a\n\
      \  -\n\
      \\n\
      \-}\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \{-|\n\
      \\n\
      \  - a\n\
      \\n\
      \-}\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \"
    ),
    ( "unordered list item containing only an asterisk",
      "module X exposing (x)\n\
      \\n\
      \{-|\n\
      \\n\
      \  - a\n\
      \  - *\n\
      \\n\
      \-}\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \{-|\n\
      \\n\
      \  - a\n\
      \\n\
      \-}\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \"
    ),
    ( "trailing space after opening in doc comment with unordered list",
      "module X exposing (x)\n\
      \\n\
      \{-| \n\
      \\n\
      \- a\n\
      \\n\
      \-}\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \{-|\n\
      \\n\
      \  - a\n\
      \\n\
      \-}\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \"
    ),
    ( "numbered list item with one asterisk in it",
      "module X exposing (x)\n\
      \\n\
      \{-|\n\
      \\n\
      \1.  *\n\
      \\n\
      \-}\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \{-| -}\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \"
    ),
    ( "two asterisks in unordered list item",
      "module X exposing (x)\n\
      \\n\
      \{-|\n\
      \\n\
      \  - **\n\
      \\n\
      \-}\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \{-|\n\
      \\n\
      \  - \\*\\*\n\
      \\n\
      \-}\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \"
    ),
    ( "two asterisks in numbered list item",
      "module X exposing (x)\n\
      \\n\
      \{-|\n\
      \\n\
      \1.  **\n\
      \\n\
      \-}\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \{-|\n\
      \\n\
      \1.  \\*\\*\n\
      \\n\
      \-}\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \"
    ),
    ( "text on next line after header after header",
      "module X exposing (x)\n\
      \\n\
      \{-|\n\
      \\n\
      \\n\
      \#\n\
      \\n\
      \\n\
      \#\n\
      \a\n\
      \\n\
      \-}\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \{-|\n\
      \\n\
      \\n\
      \#\n\
      \\n\
      \\n\
      \#\n\
      \\n\
      \a\n\
      \\n\
      \-}\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \"
    ),
    ( "two asterisks in single line doc comment",
      "module X exposing (x)\n\
      \\n\
      \{-| ** -}\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \{-| \\*\\*\n\
      \-}\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \"
    ),
    ( "two escaped asterisks in unordered list item",
      "module X exposing (x)\n\
      \\n\
      \{-|\n\
      \\n\
      \  - \\*\\*\n\
      \\n\
      \-}\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \{-|\n\
      \\n\
      \  - \\*\\*\n\
      \\n\
      \-}\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \"
    ),
    ( "two escaped asterisks in numbered list item",
      "module X exposing (x)\n\
      \\n\
      \{-|\n\
      \\n\
      \1.  \\*\\*\n\
      \\n\
      \-}\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \{-|\n\
      \\n\
      \1.  \\*\\*\n\
      \\n\
      \-}\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \"
    ),
    ( "empty single line block quote inside doc comment",
      "module X exposing (x)\n\
      \\n\
      \{-|>-}\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \{-| -}\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \"
    ),
    ( "single line non empty block quote",
      "module X exposing (x)\n\
      \\n\
      \{-|>a-}\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \{-|\n\
      \\n\
      \> a\n\
      \\n\
      \-}\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \"
    ),
    ( "formatted doc comment containing only a single line block quote",
      "module X exposing (x)\n\
      \\n\
      \{-|\n\
      \\n\
      \> a\n\
      \\n\
      \-}\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \{-|\n\
      \\n\
      \> a\n\
      \\n\
      \-}\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \"
    ),
    ( "code block preceded by text but with no empty line",
      "module X exposing (x)\n\
      \\n\
      \{-| a\n\
      \> b\n\
      \\n\
      \-}\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \{-| a\n\
      \\n\
      \> b\n\
      \\n\
      \-}\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \"
    ),
    ( "double space in block quote text in doc comment",
      "module X exposing (x)\n\
      \\n\
      \{-|\n\
      \\n\
      \> a  b\n\
      \\n\
      \-}\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \{-|\n\
      \\n\
      \> a b\n\
      \\n\
      \-}\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \"
    ),
    ( "double space in unordered list item text",
      "module X exposing (x)\n\
      \\n\
      \{-|\n\
      \\n\
      \  - a  b\n\
      \\n\
      \-}\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \{-|\n\
      \\n\
      \  - a b\n\
      \\n\
      \-}\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \"
    ),
    ( "multi line block quote in doc comment",
      "module X exposing (x)\n\
      \\n\
      \{-|\n\
      \\n\
      \> a\n\
      \> b\n\
      \\n\
      \-}\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \{-|\n\
      \\n\
      \> a\n\
      \> b\n\
      \\n\
      \-}\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \"
    ),
    ( "block quote following header in doc comment",
      "module X exposing (x)\n\
      \\n\
      \{-|\n\
      \\n\
      \\n\
      \#\n\
      \\n\
      \> a\n\
      \\n\
      \-}\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \{-|\n\
      \\n\
      \\n\
      \#\n\
      \\n\
      \> a\n\
      \\n\
      \-}\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \"
    ),
    ( "block quote followed by header in doc comment",
      "module X exposing (x)\n\
      \\n\
      \{-|\n\
      \\n\
      \> a\n\
      \\n\
      \\n\
      \#\n\
      \\n\
      \-}\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \{-|\n\
      \\n\
      \> a\n\
      \\n\
      \\n\
      \#\n\
      \\n\
      \-}\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \"
    ),
    ( "block quote on last line of doc comment",
      "module X exposing (x)\n\
      \\n\
      \{-|\n\
      \\n\
      \> a\n\
      \-}\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \{-|\n\
      \\n\
      \> a\n\
      \\n\
      \-}\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \"
    ),
    ( "convert asterisk bolding to underscore bolding",
      "module X exposing (x)\n\
      \\n\
      \{-| *a*\n\
      \-}\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \{-| _a_\n\
      \-}\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \"
    ),
    ( "unequal numbers of asterisks around text in block comment",
      "module X exposing (x)\n\
      \\n\
      \{-| **a*\n\
      \-}\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \{-| \\*\\*a\\*\n\
      \-}\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \"
    ),
    ( "asterisk bolding in block quote in doc comment",
      "module X exposing (x)\n\
      \\n\
      \{-|\n\
      \\n\
      \> *a*\n\
      \\n\
      \-}\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \{-|\n\
      \\n\
      \> _a_\n\
      \\n\
      \-}\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \"
    ),
    ( "single empty line between at docs statements",
      "module X exposing (x)\n\
      \\n\
      \{-|\n\
      \\n\
      \@docs a\n\
      \\n\
      \@docs b\n\
      \\n\
      \-}\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \{-|\n\
      \\n\
      \@docs a\n\
      \\n\
      \@docs b\n\
      \\n\
      \-}\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \"
    ),
    ( "superfluous but not identical imports",
      "module X exposing (x)\n\
      \\n\
      \import A\n\
      \import A exposing (B)\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \import A exposing (B)\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \"
    ),
    ( "two imports that should be combined",
      "module X exposing (x)\n\
      \\n\
      \import A exposing (C)\n\
      \import A as B\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \import A as B exposing (C)\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \"
    ),
    ( "comment on combinable import",
      "module X exposing (x)\n\
      \\n\
      \import A exposing\n\
      \    (B -- x\n\
      \    )\n\
      \import A exposing (C)\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \import A\n\
      \    exposing\n\
      \        ( B\n\
      \          -- x\n\
      \        , C\n\
      \        )\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \"
    ),
    ( "two line comments in top level of let in expression",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    let\n\
      \        a =\n\
      \            0\n\
      \\n\
      \        --\n\
      \        --\n\
      \    in\n\
      \    a\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    let\n\
      \        a =\n\
      \            0\n\
      \\n\
      \        --\n\
      \        --\n\
      \    in\n\
      \    a\n\
      \"
    ),
    ( "single line if then else after left pizza",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    a <| if b then c else d\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    a <|\n\
      \        if b then\n\
      \            c\n\
      \\n\
      \        else\n\
      \            d\n\
      \"
    ),
    ( "if then else following double plus infix operator",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    a ++ if b then c else d\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    a\n\
      \        ++ (if b then\n\
      \                c\n\
      \\n\
      \            else\n\
      \                d\n\
      \           )\n\
      \"
    ),
    ( "if then else preceded by right pizza operator",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    a |> if b then c else d\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    a\n\
      \        |> (if b then\n\
      \                c\n\
      \\n\
      \            else\n\
      \                d\n\
      \           )\n\
      \"
    ),
    ( "if then else preceded by less than operator",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    a < if b then c else d\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    a\n\
      \        < (if b then\n\
      \            c\n\
      \\n\
      \           else\n\
      \            d\n\
      \          )\n\
      \"
    ),
    ( "if then else preceded by greater than operator",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    a > if b then c else d\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    a\n\
      \        > (if b then\n\
      \            c\n\
      \\n\
      \           else\n\
      \            d\n\
      \          )\n\
      \"
    ),
    ( "if then else preceded by minus operator",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    a - if b then c else d\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    a\n\
      \        - (if b then\n\
      \            c\n\
      \\n\
      \           else\n\
      \            d\n\
      \          )\n\
      \"
    ),
    ( "triple backtick code block that doesn't contain elm",
      "module X exposing (x)\n\
      \\n\
      \{-|\n\
      \\n\
      \1.  a\n\
      \\n\
      \```\n\
      \c =\n\
      \  { ... }\n\
      \```\n\
      \\n\
      \-}\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \{-|\n\
      \\n\
      \1.  a\n\
      \\n\
      \```\n\
      \c =\n\
      \  { ... }\n\
      \```\n\
      \\n\
      \-}\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \"
    ),
    ( "text enclosed in double asterisks in doc comment",
      "module X exposing (x)\n\
      \\n\
      \{-| **a**\n\
      \-}\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \{-| **a**\n\
      \-}\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \"
    ),
    ( "text surrounded by double asterisks in unordered list item",
      "module X exposing (x)\n\
      \\n\
      \{-|\n\
      \\n\
      \  - **a**\n\
      \\n\
      \-}\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \{-|\n\
      \\n\
      \  - **a**\n\
      \\n\
      \-}\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \"
    ),
    ( "code block following header",
      "module X exposing (x)\n\
      \\n\
      \{-|\n\
      \\n\
      \\n\
      \#\n\
      \\n\
      \    a =\n\
      \        0\n\
      \\n\
      \-}\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \{-|\n\
      \\n\
      \\n\
      \#\n\
      \\n\
      \    a =\n\
      \        0\n\
      \\n\
      \-}\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \"
    ),
    ( "at docs followed by code block",
      "module X exposing (x)\n\
      \\n\
      \{-|\n\
      \\n\
      \@docs x\n\
      \\n\
      \    a\n\
      \\n\
      \-}\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \{-|\n\
      \\n\
      \@docs x\n\
      \\n\
      \    a\n\
      \\n\
      \-}\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \"
    ),
    ( "code block following header following at docs",
      "module X exposing (x)\n\
      \\n\
      \{-|\n\
      \\n\
      \@docs x\n\
      \\n\
      \\n\
      \#\n\
      \\n\
      \    a\n\
      \\n\
      \-}\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \{-|\n\
      \\n\
      \@docs x\n\
      \\n\
      \\n\
      \#\n\
      \\n\
      \    a\n\
      \\n\
      \-}\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \"
    ),
    ( "bolded hyphen",
      "module X exposing (x)\n\
      \\n\
      \{-| **-**\n\
      \-}\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \{-| **-**\n\
      \-}\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \"
    ),
    ( "multi line unordered list item followed by a single line one",
      "module X exposing (x)\n\
      \\n\
      \{-|\n\
      \\n\
      \  - a\n\
      \    b\n\
      \\n\
      \  - c\n\
      \\n\
      \-}\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \{-|\n\
      \\n\
      \  - a\n\
      \    b\n\
      \\n\
      \  - c\n\
      \\n\
      \-}\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \"
    ),
    ( "second line in unordered list item",
      "module X exposing (x)\n\
      \\n\
      \{-|\n\
      \\n\
      \  - a\n\
      \    b\n\
      \  - c\n\
      \\n\
      \-}\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \{-|\n\
      \\n\
      \  - a\n\
      \    b\n\
      \  - c\n\
      \\n\
      \-}\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \"
    ),
    ( "trailing empty line in multi line doc comment",
      "module X exposing (x)\n\
      \\n\
      \{-| a\n\
      \b\n\
      \\n\
      \-}\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \{-| a\n\
      \b\n\
      \-}\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \"
    ),
    ( "only line comment in code block in doc comment",
      "module X exposing (x)\n\
      \\n\
      \{-|\n\
      \\n\
      \    --\n\
      \\n\
      \\n\
      \\n\
      \-}\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \{-|\n\
      \\n\
      \    --\n\
      \\n\
      \\n\
      \\n\
      \-}\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \"
    ),
    ( "only non empty line comment in doc comment code block",
      "module X exposing (x)\n\
      \\n\
      \{-|\n\
      \\n\
      \    -- a\n\
      \\n\
      \\n\
      \\n\
      \-}\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \{-|\n\
      \\n\
      \    -- a\n\
      \\n\
      \\n\
      \\n\
      \-}\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \"
    ),
    ( "non elm code block with trailing line comment",
      "module X exposing (x)\n\
      \\n\
      \{-|\n\
      \\n\
      \    a\n\
      \    --\n\
      \\n\
      \-}\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \{-|\n\
      \\n\
      \    a\n\
      \    --\n\
      \\n\
      \-}\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \"
    ),
    ( "empty line comment in code block following ordinary text",
      "module X exposing (x)\n\
      \\n\
      \{-| a\n\
      \\n\
      \    --\n\
      \\n\
      \\n\
      \\n\
      \-}\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \{-| a\n\
      \\n\
      \    --\n\
      \\n\
      \\n\
      \\n\
      \-}\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \"
    ),
    ( "non elm code block containing line comment following ordinary text",
      "module X exposing (x)\n\
      \\n\
      \{-| a\n\
      \\n\
      \    b\n\
      \    --\n\
      \\n\
      \-}\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \{-| a\n\
      \\n\
      \    b\n\
      \    --\n\
      \\n\
      \-}\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \"
    ),
    ( "doc comment containing a single backslash",
      "module X exposing (x)\n\
      \\n\
      \{-| \\\n\
      \-}\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \{-| \\\\\n\
      \-}\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \"
    ),
    ( "escaped backslash in unordered list",
      "module X exposing (x)\n\
      \\n\
      \{-|\n\
      \\n\
      \  - \\\n\
      \\n\
      \-}\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \{-|\n\
      \\n\
      \  - \\\\\n\
      \\n\
      \-}\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \"
    ),
    ( "single backslash in numbered list item",
      "module X exposing (x)\n\
      \\n\
      \{-|\n\
      \\n\
      \1.  \\\n\
      \\n\
      \-}\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \{-|\n\
      \\n\
      \1.  \\\\\n\
      \\n\
      \-}\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \"
    ),
    ( "escaped underscore in doc comment",
      "module X exposing (x)\n\
      \\n\
      \{-| \\_\n\
      \-}\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \{-| \\_\n\
      \-}\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \"
    ),
    ( "unescaped underscore in doc comment",
      "module X exposing (x)\n\
      \\n\
      \{-| _\n\
      \-}\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \{-| \\_\n\
      \-}\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \"
    ),
    ( "doc comment containing a single backtick",
      "module X exposing (x)\n\
      \\n\
      \{-| `\n\
      \-}\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \{-| \\`\n\
      \-}\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \"
    ),
    ( "text enclosed in single backticks in doc comment",
      "module X exposing (x)\n\
      \\n\
      \{-| `a`\n\
      \-}\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \{-| `a`\n\
      \-}\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \"
    ),
    ( "double backtick quote in doc comment",
      "module X exposing (x)\n\
      \\n\
      \{-| ``a``\n\
      \-}\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \{-| `a`\n\
      \-}\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \"
    ),
    ( "quoted backtick in doc comment",
      "module X exposing (x)\n\
      \\n\
      \{-| \\`\n\
      \-}\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \{-| \\`\n\
      \-}\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \"
    ),
    ( "many function parameters",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x a b c d e f g h j k l m n o p q r =\n\
      \    0\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x a b c d e f g h j k l m n o p q r =\n\
      \    0\n\
      \"
    ),
    ( "underscore in backticks in doc comment",
      "module X exposing (x)\n\
      \\n\
      \{-| `_`\n\
      \-}\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \{-| `_`\n\
      \-}\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \"
    ),
    ( "asterisk in backticks in doc comment",
      "module X exposing (x)\n\
      \\n\
      \{-| `*`\n\
      \-}\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \{-| `*`\n\
      \-}\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \"
    ),
    ( "backticked text following underscore bolded text",
      "module X exposing (x)\n\
      \\n\
      \{-| _a_ `b`\n\
      \-}\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \{-| _a_ `b`\n\
      \-}\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \"
    ),
    ( "backtick quoted text following text in doc comment",
      "module X exposing (x)\n\
      \\n\
      \{-| a `_`\n\
      \-}\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \{-| a `_`\n\
      \-}\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \"
    ),
    ( "asterisk in backticks following text in doc comment",
      "module X exposing (x)\n\
      \\n\
      \{-| a `*`\n\
      \-}\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \{-| a `*`\n\
      \-}\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \"
    ),
    ( "single backtick in unordered list item",
      "module X exposing (x)\n\
      \\n\
      \{-|\n\
      \\n\
      \  - `\n\
      \\n\
      \-}\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \{-|\n\
      \\n\
      \  - \\`\n\
      \\n\
      \-}\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \"
    ),
    ( "single backtick in numbered list item",
      "module X exposing (x)\n\
      \\n\
      \{-|\n\
      \\n\
      \1.  `\n\
      \\n\
      \-}\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \{-|\n\
      \\n\
      \1.  \\`\n\
      \\n\
      \-}\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \"
    ),
    ( "https url in doc comment",
      "module X exposing (x)\n\
      \\n\
      \{-| https://a.com\n\
      \-}\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \{-| <https://a.com>\n\
      \-}\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \"
    ),
    ( "http url in doc comment",
      "module X exposing (x)\n\
      \\n\
      \{-| http://a.com\n\
      \-}\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \{-| <http://a.com>\n\
      \-}\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \"
    ),
    ( "aaa url in doc comment",
      "module X exposing (x)\n\
      \\n\
      \{-| aaa://a.com\n\
      \-}\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \{-| <aaa://a.com>\n\
      \-}\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \"
    ),
    ( "acap url in doc comment",
      "module X exposing (x)\n\
      \\n\
      \{-| acap:a\n\
      \-}\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \{-| <acap:a>\n\
      \-}\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \"
    ),
    ( "adiumxtra url in doc comment",
      "module X exposing (x)\n\
      \\n\
      \{-| adiumxtra:a\n\
      \-}\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \{-| <adiumxtra:a>\n\
      \-}\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \"
    ),
    ( "afp url in doc comment",
      "module X exposing (x)\n\
      \\n\
      \{-| afp:a\n\
      \-}\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \{-| <afp:a>\n\
      \-}\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \"
    ),
    ( "afs url in doc comment",
      "module X exposing (x)\n\
      \\n\
      \{-| afs:a\n\
      \-}\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \{-| <afs:a>\n\
      \-}\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \"
    ),
    ( "aim url in doc comment",
      "module X exposing (x)\n\
      \\n\
      \{-| aim:a\n\
      \-}\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \{-| <aim:a>\n\
      \-}\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \"
    ),
    ( "apt URL in doc comment",
      "module X exposing (x)\n\
      \\n\
      \{-| apt:a\n\
      \-}\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \{-| <apt:a>\n\
      \-}\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \"
    ),
    ( "attachment URL in doc comment",
      "module X exposing (x)\n\
      \\n\
      \{-| attachment:a\n\
      \-}\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \{-| <attachment:a>\n\
      \-}\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \"
    ),
    ( "aw url in doc comment",
      "module X exposing (x)\n\
      \\n\
      \{-| aw:a\n\
      \-}\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \{-| <aw:a>\n\
      \-}\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \"
    ),
    ( "beshare url in doc comment",
      "module X exposing (x)\n\
      \\n\
      \{-| beshare:a\n\
      \-}\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \{-| <beshare:a>\n\
      \-}\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \"
    ),
    ( "bitcoin url in doc comment",
      "module X exposing (x)\n\
      \\n\
      \{-| bitcoin:a\n\
      \-}\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \{-| <bitcoin:a>\n\
      \-}\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \"
    ),
    ( "bolo URL in doc comment",
      "module X exposing (x)\n\
      \\n\
      \{-| bolo:a\n\
      \-}\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \{-| <bolo:a>\n\
      \-}\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \"
    ),
    ( "callto url in doc comment",
      "module X exposing (x)\n\
      \\n\
      \{-| callto:a\n\
      \-}\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \{-| <callto:a>\n\
      \-}\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \"
    ),
    ( "cap url in doc comment",
      "module X exposing (x)\n\
      \\n\
      \{-| cap:a\n\
      \-}\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \{-| <cap:a>\n\
      \-}\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \"
    ),
    ( "chrome url in doc comment",
      "module X exposing (x)\n\
      \\n\
      \{-| chrome:a\n\
      \-}\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \{-| <chrome:a>\n\
      \-}\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \"
    ),
    ( "cid url in doc comment",
      "module X exposing (x)\n\
      \\n\
      \{-| cid:a\n\
      \-}\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \{-| <cid:a>\n\
      \-}\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \"
    ),
    ( "coap url in doc comment",
      "module X exposing (x)\n\
      \\n\
      \{-| coap:a\n\
      \-}\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \{-| <coap:a>\n\
      \-}\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \"
    ),
    ( "content url in doc comment",
      "module X exposing (x)\n\
      \\n\
      \{-| content:a\n\
      \-}\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \{-| <content:a>\n\
      \-}\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \"
    ),
    ( "crid url in doc comment",
      "module X exposing (x)\n\
      \\n\
      \{-| crid:a\n\
      \-}\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \{-| <crid:a>\n\
      \-}\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \"
    ),
    ( "cvs url in doc comment",
      "module X exposing (x)\n\
      \\n\
      \{-| cvs:a\n\
      \-}\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \{-| <cvs:a>\n\
      \-}\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \"
    ),
    ( "data url in doc comment",
      "module X exposing (x)\n\
      \\n\
      \{-| data:a\n\
      \-}\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \{-| <data:a>\n\
      \-}\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \"
    ),
    ( "dav url in doc comment",
      "module X exposing (x)\n\
      \\n\
      \{-| dav:a\n\
      \-}\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \{-| <dav:a>\n\
      \-}\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \"
    ),
    ( "dict url in doc comment",
      "module X exposing (x)\n\
      \\n\
      \{-| dict:a\n\
      \-}\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \{-| <dict:a>\n\
      \-}\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \"
    ),
    ( "dns url in doc comment",
      "module X exposing (x)\n\
      \\n\
      \{-| dns:a\n\
      \-}\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \{-| <dns:a>\n\
      \-}\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \"
    ),
    ( "doi url in doc comment",
      "module X exposing (x)\n\
      \\n\
      \{-| doi:a\n\
      \-}\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \{-| <doi:a>\n\
      \-}\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \"
    ),
    ( "go in a doc comment",
      "module X exposing (x)\n\
      \\n\
      \{-| go\n\
      \-}\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \{-| go\n\
      \-}\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \"
    ),
    ( "link alias in doc comment",
      "module X exposing (x)\n\
      \\n\
      \{-|\n\
      \\n\
      \[a]: b\n\
      \\n\
      \-}\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \{-|\n\
      \\n\
      \[a]: b\n\
      \\n\
      \-}\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \"
    ),
    ( "line starting with open bracket in doc comment",
      "module X exposing (x)\n\
      \\n\
      \{-| a\n\
      \\n\
      \[\n\
      \\n\
      \-}\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \{-| a\n\
      \\n\
      \[\n\
      \\n\
      \-}\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \"
    ),
    ( "underscore in url",
      "module X exposing (x)\n\
      \\n\
      \{-| [a](_)\n\
      \-}\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \{-| [a](_)\n\
      \-}\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \"
    ),
    ( "ordinary text in doc comment followed by underscore in url",
      "module X exposing (x)\n\
      \\n\
      \{-| a [b](_)\n\
      \-}\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \{-| a [b](_)\n\
      \-}\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \"
    ),
    ( "doc comment containing only a square bracket",
      "module X exposing (x)\n\
      \\n\
      \{-| [\n\
      \-}\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \{-| [\n\
      \-}\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \"
    ),
    ( "backticked text followed by underscore bolded text",
      "module X exposing (x)\n\
      \\n\
      \{-| `a` _b_\n\
      \-}\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \{-| `a` _b_\n\
      \-}\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \"
    ),
    ( "backticked text followed by double asterisk text",
      "module X exposing (x)\n\
      \\n\
      \{-| `a` **b**\n\
      \-}\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \{-| `a` **b**\n\
      \-}\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \"
    ),
    ( "block comment with two hyphens at the start",
      "module X exposing (x)\n\
      \\n\
      \{--\n\
      \a\n\
      \-}\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \{--\n\
      \a\n\
      \-}\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \"
    ),
    ( "doc comment where the second line ends with a dot",
      "module X exposing (x)\n\
      \\n\
      \{-| a\n\
      \b.\n\
      \-}\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \{-| a\n\
      \b.\n\
      \-}\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \"
    ),
    ( "doc comment ended with open square bracket",
      "module X exposing (x)\n\
      \\n\
      \{-| a\n\
      \[\n\
      \-}\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \{-| a\n\
      \[\n\
      \-}\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \"
    ),
    ( "doc comment ending in two type aliases",
      "module X exposing (x)\n\
      \\n\
      \{-|\n\
      \\n\
      \[a]: b\n\
      \[c]: d\n\
      \\n\
      \-}\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \{-|\n\
      \\n\
      \[a]: b\n\
      \[c]: d\n\
      \\n\
      \-}\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \"
    ),
    ( "doc comment containing only an open square bracket",
      "module X exposing (x)\n\
      \\n\
      \{-|\n\
      \\n\
      \[\n\
      \\n\
      \-}\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \{-| [\n\
      \-}\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \"
    ),
    ( "header following code block",
      "module X exposing (x)\n\
      \\n\
      \{-|\n\
      \\n\
      \    a\n\
      \\n\
      \\n\
      \#\n\
      \\n\
      \-}\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \{-|\n\
      \\n\
      \    a\n\
      \\n\
      \\n\
      \#\n\
      \\n\
      \-}\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \"
    ),
    ( "pattern match with argument in brackets in cons in cons",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    case a of\n\
      \        ((B c) :: d) :: e ->\n\
      \            f\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    case a of\n\
      \        ((B c) :: d) :: e ->\n\
      \            f\n\
      \"
    ),
    ( "text followed by at sign in doc comment",
      "module X exposing (x)\n\
      \\n\
      \{-| a @\n\
      \-}\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \{-| a @\n\
      \-}\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \"
    ),
    ( "at docs following extra at sign",
      "module X exposing (b, a)\n\
      \\n\
      \{-| @\n\
      \\n\
      \@docs b, a\n\
      \\n\
      \-}\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \",
      "module X exposing (b, a)\n\
      \\n\
      \{-| @\n\
      \\n\
      \@docs b, a\n\
      \\n\
      \-}\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \"
    ),
    ( "at symbol in backticks in doc comment",
      "module X exposing (x)\n\
      \\n\
      \{-| `@`\n\
      \-}\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \{-| `@`\n\
      \-}\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \"
    ),
    ( "typed backticked code block in doc comment",
      "module X exposing (x)\n\
      \\n\
      \{-|\n\
      \\n\
      \```a\n\
      \b\n\
      \```\n\
      \\n\
      \-}\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \{-|\n\
      \\n\
      \```a\n\
      \b\n\
      \```\n\
      \\n\
      \-}\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \"
    ),
    ( "typed code block in doc comment followed by text",
      "module X exposing (x)\n\
      \\n\
      \{-|\n\
      \\n\
      \```a\n\
      \b\n\
      \```\n\
      \\n\
      \c\n\
      \\n\
      \-}\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \{-|\n\
      \\n\
      \```a\n\
      \b\n\
      \```\n\
      \\n\
      \c\n\
      \\n\
      \-}\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \"
    ),
    ( "double hyphens on each end of block comment",
      "module X exposing (x)\n\
      \\n\
      \{----}\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \{----}\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \"
    ),
    ( "text with no spaces in double hyphen block quote",
      "module X exposing (x)\n\
      \\n\
      \{--a--}\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \{--a--}\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \"
    ),
    ( "space after text in double hyphen block comment",
      "module X exposing (x)\n\
      \\n\
      \{--a --}\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \{--a --}\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \"
    ),
    ( "url scheme followed by colon then whitespace",
      "module X exposing (x)\n\
      \\n\
      \{-| notes:\n\
      \-}\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \{-| notes:\n\
      \-}\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \"
    ),
    ( "backtick quote containing only a space in doc comment",
      "module X exposing (x)\n\
      \\n\
      \{-| ` `\n\
      \-}\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \{-| \\`\\`\n\
      \-}\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \"
    ),
    ( "backtick quote containing backslash and space in doc comment",
      "module X exposing (x)\n\
      \\n\
      \{-| `\\ `\n\
      \-}\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \{-| `\\`\n\
      \-}\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \"
    ),
    ( "backslash in backticks following text in doc comment",
      "module X exposing (x)\n\
      \\n\
      \{-| a`\\ `\n\
      \-}\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \{-| a`\\`\n\
      \-}\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \"
    ),
    ( "elm code block in doc comment with five space indent",
      "module X exposing (x)\n\
      \\n\
      \{-|\n\
      \\n\
      \     a = 0\n\
      \\n\
      \-}\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \{-|\n\
      \\n\
      \     a = 0\n\
      \\n\
      \-}\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \"
    ),
    ( "doc comment containing only an empty line comment",
      "module X exposing (x)\n\
      \\n\
      \{-|\n\
      \--\n\
      \-}\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \{-| --\n\
      \-}\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \"
    ),
    ( "line in doc comment starting with two hyphens and greater than",
      "module X exposing (x)\n\
      \\n\
      \{-|\n\
      \\n\
      \-->\n\
      \\n\
      \-}\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \{-|\n\
      \\n\
      \-->\n\
      \\n\
      \-}\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \"
    ),
    ( "text followed by hyphen on new line in doc comment",
      "module X exposing (x)\n\
      \\n\
      \{-| a\n\
      \-\n\
      \-}\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \{-| a\n\
      \-}\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \"
    ),
    ( "underline second level header",
      "module X exposing (x)\n\
      \\n\
      \{-| a\n\
      \--\n\
      \-}\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \{-|\n\
      \\n\
      \\n\
      \## a\n\
      \\n\
      \-}\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \"
    ),
    ( "underline top level header",
      "module X exposing (x)\n\
      \\n\
      \{-| a\n\
      \==\n\
      \\n\
      \-}\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \{-|\n\
      \\n\
      \\n\
      \# a\n\
      \\n\
      \-}\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \"
    ),
    ( "arrow on line after text in doc comment",
      "module X exposing (x)\n\
      \\n\
      \{-| a\n\
      \--b\n\
      \-}\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \{-| a\n\
      \--b\n\
      \-}\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \"
    ),
    ( "doc comment containing only an asterisk",
      "module X exposing (x)\n\
      \\n\
      \{-| *\n\
      \-}\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \{-| -}\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \"
    ),
    ( "unordered list using the plus symbol",
      "module X exposing (x)\n\
      \\n\
      \{-|\n\
      \\n\
      \  + a\n\
      \  + b\n\
      \\n\
      \-}\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \{-|\n\
      \\n\
      \  - a\n\
      \  - b\n\
      \\n\
      \-}\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \"
    ),
    ( "multiline asterisk bold",
      "module X exposing (x)\n\
      \\n\
      \{-| **a\n\
      \**\n\
      \-}\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \{-| **a\n\
      \**\n\
      \-}\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \"
    ),
    ( "multiline asterisk bold after text in doc comment",
      "module X exposing (x)\n\
      \\n\
      \{-| a **b\n\
      \**\n\
      \-}\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \{-| a **b\n\
      \**\n\
      \-}\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \"
    ),
    ( "bolded text in type declaration after multi line empty module docs",
      "module X exposing (x)\n\
      \\n\
      \{-|\n\
      \-}\n\
      \\n\
      \\n\
      \type X\n\
      \    = {-*a*-} A\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \{-| -}\n\
      \\n\
      \\n\
      \type X\n\
      \    = {- *a* -} A\n\
      \"
    ),
    ( "block comment containing single hyphen in backticks",
      "module X exposing (x)\n\
      \\n\
      \{- `-` -}\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \{- `-` -}\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \"
    ),
    ( "asterisk bolding followed by text",
      "module X exposing (x)\n\
      \\n\
      \{-| **a** b\n\
      \-}\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \{-| **a** b\n\
      \-}\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \"
    ),
    ( "multiline asterisk bold followed by space and text",
      "module X exposing (x)\n\
      \\n\
      \{-| **a\n\
      \** b\n\
      \-}\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \{-| **a\n\
      \** b\n\
      \-}\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \"
    ),
    ( "hyphen in backticks followed by multiline asterisk bold",
      "module X exposing (x)\n\
      \\n\
      \{-| `-` **a\n\
      \**\n\
      \-}\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \{-| `-` **a\n\
      \**\n\
      \-}\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \"
    ),
    ( "multiline double asterisk text after single asterisked text",
      "module X exposing (x)\n\
      \\n\
      \{-| *a* **b\n\
      \**\n\
      \-}\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \{-| _a_ **b\n\
      \**\n\
      \-}\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \"
    ),
    ( "single asterisked then text then double multiline asterisked",
      "module X exposing (x)\n\
      \\n\
      \{-| *a* b **c\n\
      \**\n\
      \-}\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \{-| _a_ b **c\n\
      \**\n\
      \-}\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \"
    ),
    ( "asterisk unordered list item",
      "module X exposing (x)\n\
      \\n\
      \{-|\n\
      \\n\
      \  * a\n\
      \\n\
      \-}\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \{-|\n\
      \\n\
      \  - a\n\
      \\n\
      \-}\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \"
    ),
    ( "gappy unordered list with asterisk bullets",
      "module X exposing (x)\n\
      \\n\
      \{-|\n\
      \\n\
      \  * a\n\
      \\n\
      \  * b\n\
      \  * c\n\
      \\n\
      \-}\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \{-|\n\
      \\n\
      \  - a\n\
      \\n\
      \  - b\n\
      \\n\
      \  - c\n\
      \\n\
      \-}\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \"
    ),
    ( "empty line in unordered list item",
      "module X exposing (x)\n\
      \\n\
      \{-|\n\
      \\n\
      \  - a\n\
      \\n\
      \    b\n\
      \\n\
      \-}\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \{-|\n\
      \\n\
      \  - a\n\
      \\n\
      \    b\n\
      \\n\
      \-}\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \"
    ),
    ( "multiline double asterisk quote after backticked after text",
      "module X exposing (x)\n\
      \\n\
      \{-| a `-` **b\n\
      \**\n\
      \-}\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \{-| a `-` **b\n\
      \**\n\
      \-}\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \"
    ),
    ( "line comment after new type name",
      "module X exposing (X)\n\
      \\n\
      \\n\
      \type\n\
      \    X\n\
      \    --\n\
      \    = X\n\
      \",
      "module X exposing (X)\n\
      \\n\
      \\n\
      \type\n\
      \    X\n\
      \    --\n\
      \    = X\n\
      \"
    ),
    ( "line comment between type keyword and type name",
      "module X exposing (X)\n\
      \\n\
      \\n\
      \type\n\
      \    --\n\
      \    X\n\
      \    = X\n\
      \",
      "module X exposing (X)\n\
      \\n\
      \\n\
      \type\n\
      \    --\n\
      \    X\n\
      \    = X\n\
      \"
    ),
    ( "line comment immediately after type keyword",
      "module X exposing (X)\n\
      \\n\
      \\n\
      \type--\n\
      \    X\n\
      \    = X\n\
      \",
      "module X exposing (X)\n\
      \\n\
      \\n\
      \type\n\
      \    --\n\
      \    X\n\
      \    = X\n\
      \"
    ),
    ( "block comment immediately after type keyword",
      "module X exposing (X)\n\
      \\n\
      \\n\
      \type{--}X = X\n\
      \",
      "module X exposing (X)\n\
      \\n\
      \\n\
      \type\n\
      \    {--}\n\
      \    X\n\
      \    = X\n\
      \"
    ),
    ( "block comment between new type name and equals",
      "module X exposing (X)\n\
      \\n\
      \\n\
      \type X{--}= X\n\
      \",
      "module X exposing (X)\n\
      \\n\
      \\n\
      \type\n\
      \    X\n\
      \    {--}\n\
      \    = X\n\
      \"
    ),
    ( "block comment after equals in type declaration",
      "module X exposing (X)\n\
      \\n\
      \\n\
      \type X = {--} X\n\
      \",
      "module X exposing (X)\n\
      \\n\
      \\n\
      \type X\n\
      \    = {--}\n\
      \      X\n\
      \"
    ),
    ( "block comment after final branch of type declaration",
      "module X exposing (X)\n\
      \\n\
      \\n\
      \type X\n\
      \    = X {- a -}\n\
      \",
      "module X exposing (X)\n\
      \\n\
      \\n\
      \type X\n\
      \    = X\n\
      \\n\
      \\n\
      \\n\
      \{- a -}\n\
      \"
    ),
    ( "line and block comment following final type branch",
      "module X exposing (X)\n\
      \\n\
      \\n\
      \type X\n\
      \    = X --\n\
      \{- a -}\n\
      \",
      "module X exposing (X)\n\
      \\n\
      \\n\
      \type X\n\
      \    = X --\n\
      \\n\
      \\n\
      \\n\
      \{- a -}\n\
      \"
    ),
    ( "multiple line comments following final type branch",
      "module X exposing (X)\n\
      \\n\
      \\n\
      \type X\n\
      \    = X --\n\
      \        --\n\
      \",
      "module X exposing (X)\n\
      \\n\
      \\n\
      \type X\n\
      \    = X --\n\
      \\n\
      \\n\
      \\n\
      \--\n\
      \"
    ),
    ( "empty block comment at end of module",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \\n\
      \\n\
      \{--}\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \\n\
      \\n\
      \{--}\n\
      \"
    ),
    ( "type branch with parameter and block and line comments",
      "module X exposing (X)\n\
      \\n\
      \\n\
      \type X a\n\
      \    = A {--} a --\n\
      \",
      "module X exposing (X)\n\
      \\n\
      \\n\
      \type X a\n\
      \    = A\n\
      \        {--}\n\
      \        a\n\
      \      --\n\
      \"
    ),
    ( "type branch with parameter and block comment and line comment",
      "module X exposing (X)\n\
      \\n\
      \\n\
      \type X\n\
      \    = A {- a -} b --\n\
      \",
      "module X exposing (X)\n\
      \\n\
      \\n\
      \type X\n\
      \    = A {- a -} b --\n\
      \"
    ),
    ( "two block comments between type constructor and parameter",
      "module X exposing (X)\n\
      \\n\
      \\n\
      \type X\n\
      \    = A {--} {--} b\n\
      \",
      "module X exposing (X)\n\
      \\n\
      \\n\
      \type X\n\
      \    = A\n\
      \        {--}\n\
      \        {--}\n\
      \        b\n\
      \"
    ),
    ( "two block comments after type branch with one full and one empty",
      "module X exposing (X)\n\
      \\n\
      \\n\
      \type X\n\
      \    = A {- a -} {--}\n\
      \",
      "module X exposing (X)\n\
      \\n\
      \\n\
      \type X\n\
      \    = A\n\
      \\n\
      \\n\
      \\n\
      \{- a -}\n\
      \\n\
      \\n\
      \{--}\n\
      \"
    ),
    ( "block comment at start of module exports",
      "module X exposing ({--}x)\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \",
      "module X exposing\n\
      \    ( {--}\n\
      \      x\n\
      \    )\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \"
    ),
    ( "block comment followed by line comment in module exports",
      "module X exposing\n\
      \    ( {--}\n\
      \      --\n\
      \      x\n\
      \    )\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \",
      "module X exposing\n\
      \    ( {--}\n\
      \      --\n\
      \      x\n\
      \    )\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \"
    ),
    ( "single line block comment in single line module exports",
      "module X exposing (x {- a -})\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \",
      "module X exposing (x {- a -})\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \"
    ),
    ( "single line block comment before and after module export",
      "module X exposing ({- a -} x {- b -})\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \",
      "module X exposing ({- a -} x {- b -})\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \"
    ),
    ( "module export followed by empty block comment",
      "module X exposing\n\
      \    ( x\n\
      \      {--}\n\
      \    )\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \",
      "module X exposing\n\
      \    ( x\n\
      \      {--}\n\
      \    )\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \"
    ),
    ( "line comment after left pizza",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    a <|\n\
      \        --\n\
      \        b\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    a <|\n\
      \        --\n\
      \        b\n\
      \"
    ),
    ( "empty block comment after left pizza",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    a <| {--} b\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    a <|\n\
      \        {--}\n\
      \        b\n\
      \"
    ),
    ( "empty block comment before left pizza",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    a {--} <| b\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    a\n\
      \    {--}\n\
      \    <|\n\
      \        b\n\
      \"
    ),
    ( "multi line block comment with no newline before the end",
      "module X exposing (x)\n\
      \\n\
      \{-\n\
      \   a -}\n\
      \\n\
      \\n\
      \x =\n\
      \    b\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \{-\n\
      \   a\n\
      \-}\n\
      \\n\
      \\n\
      \x =\n\
      \    b\n\
      \"
    ),
    ( "multi line doc comment with too much indentation",
      "module X exposing (X)\n\
      \\n\
      \{-\n\
      \    a\n\
      \-}\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \",
      "module X exposing (X)\n\
      \\n\
      \{-\n\
      \   a\n\
      \-}\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \"
    ),
    ( "empty block comment before equals in function definition",
      "module X exposing (X)\n\
      \\n\
      \\n\
      \x {--} =\n\
      \    0\n\
      \",
      "module X exposing (X)\n\
      \\n\
      \\n\
      \x =\n\
      \    {--}\n\
      \    0\n\
      \"
    ),
    ( "comment before parameter in top level bind",
      "module X exposing (X)\n\
      \\n\
      \\n\
      \x {--} a =\n\
      \    0\n\
      \",
      "module X exposing (X)\n\
      \\n\
      \\n\
      \x\n\
      \    {--}\n\
      \    a\n\
      \    =\n\
      \    0\n\
      \"
    ),
    ( "non empty block comment before top level function parameter",
      "module X exposing (X)\n\
      \\n\
      \\n\
      \x {- a -} a =\n\
      \    0\n\
      \",
      "module X exposing (X)\n\
      \\n\
      \\n\
      \x {- a -} a =\n\
      \    0\n\
      \"
    ),
    ( "multi line block comment before top level function parameter",
      "module X exposing (X)\n\
      \\n\
      \\n\
      \x\n\
      \    {- a\n\
      \       b\n\
      \    -}\n\
      \    c\n\
      \    =\n\
      \    0\n\
      \",
      "module X exposing (X)\n\
      \\n\
      \\n\
      \x\n\
      \    {- a\n\
      \       b\n\
      \    -}\n\
      \    c\n\
      \    =\n\
      \    0\n\
      \"
    ),
    ( "line comment after function body",
      "module X exposing (X)\n\
      \\n\
      \\n\
      \x =\n\
      \    0--\n\
      \",
      "module X exposing (X)\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \\n\
      \\n\
      \\n\
      \--\n\
      \"
    ),
    ( "empty block comment between function name and argument",
      "module X exposing (X)\n\
      \\n\
      \\n\
      \x =\n\
      \    a{--}b\n\
      \",
      "module X exposing (X)\n\
      \\n\
      \\n\
      \x =\n\
      \    a\n\
      \        {--}\n\
      \        b\n\
      \"
    ),
    ( "asterisk unordered list with extra spaces",
      "module X exposing (x)\n\
      \\n\
      \{-\n\
      \\n\
      \   * a\n\
      \-}\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \{-\n\
      \\n\
      \   * a\n\
      \-}\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \"
    ),
    ( "unordered list item with three lines of text in it",
      "module X exposing (x)\n\
      \\n\
      \{-|\n\
      \\n\
      \  - a\n\
      \    b\n\
      \    c\n\
      \\n\
      \-}\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \{-|\n\
      \\n\
      \  - a\n\
      \    b\n\
      \    c\n\
      \\n\
      \-}\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \"
    ),
    ( "multi line text in doc comment enclosed in underscores",
      "module X exposing (x)\n\
      \\n\
      \{-| _a\n\
      \_\n\
      \-}\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \{-| _a\n\
      \_\n\
      \-}\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \"
    ),
    ( "multi line underscored text with leading text",
      "module X exposing (x)\n\
      \\n\
      \{-| a _b\n\
      \_\n\
      \-}\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \{-| a _b\n\
      \_\n\
      \-}\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \"
    ),
    ( "underscore in url in doc comment following another doc comment",
      "module X exposing (x)\n\
      \\n\
      \{-| `_`\n\
      \-}\n\
      \\n\
      \\n\
      \{-| [a](https://b.com/c_d)\n\
      \-}\n\
      \x =\n\
      \    0\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \{-| `_`\n\
      \-}\n\
      \\n\
      \\n\
      \{-| [a](https://b.com/c_d)\n\
      \-}\n\
      \x =\n\
      \    0\n\
      \"
    ),
    ( "line comment in module exports with at docs",
      "module X exposing\n\
      \    ( a, b\n\
      \    , c\n\
      \      --\n\
      \    )\n\
      \\n\
      \{-|\n\
      \\n\
      \@docs a, b\n\
      \\n\
      \-}\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \",
      "module X exposing\n\
      \    ( a, b\n\
      \    , c\n\
      \      --\n\
      \    )\n\
      \\n\
      \{-|\n\
      \\n\
      \@docs a, b\n\
      \\n\
      \-}\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \"
    ),
    ( "exports with docs and multi item non doc row with line comment",
      "module X exposing\n\
      \    ( a, b\n\
      \    , c, d\n\
      \      --\n\
      \    )\n\
      \\n\
      \{-|\n\
      \\n\
      \@docs a, b\n\
      \\n\
      \-}\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \",
      "module X exposing\n\
      \    ( a, b\n\
      \    , c, d\n\
      \      --\n\
      \    )\n\
      \\n\
      \{-|\n\
      \\n\
      \@docs a, b\n\
      \\n\
      \-}\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \"
    ),
    ( "documented exports followed by empty block comment",
      "module X exposing\n\
      \    ( a\n\
      \    , b\n\
      \    {--}\n\
      \    )\n\
      \\n\
      \{-|\n\
      \\n\
      \@docs a, b\n\
      \\n\
      \-}\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \",
      "module X exposing\n\
      \    ( a\n\
      \    , b\n\
      \    {--}\n\
      \    )\n\
      \\n\
      \{-|\n\
      \\n\
      \@docs a, b\n\
      \\n\
      \-}\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \"
    ),
    ( "non empty block comment in single line documented exports",
      "module X exposing (a, b {- a -})\n\
      \\n\
      \{-|\n\
      \\n\
      \@docs a, b\n\
      \\n\
      \-}\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \",
      "module X exposing (a, b {- a -})\n\
      \\n\
      \{-|\n\
      \\n\
      \@docs a, b\n\
      \\n\
      \-}\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \"
    ),
    ( "multi line documented exposing with non empty block comment",
      "module X exposing\n\
      \    ( a\n\
      \    , b {- a -}\n\
      \    )\n\
      \\n\
      \{-|\n\
      \\n\
      \@docs a, b\n\
      \\n\
      \-}\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \",
      "module X exposing (a, b {- a -})\n\
      \\n\
      \{-|\n\
      \\n\
      \@docs a, b\n\
      \\n\
      \-}\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \"
    ),
    ( "single documented export with empty block comment",
      "module X exposing ( a {--})\n\
      \\n\
      \{-|\n\
      \\n\
      \@docs a\n\
      \\n\
      \-}\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \",
      "module X exposing\n\
      \    ( a\n\
      \    {--}\n\
      \    )\n\
      \\n\
      \{-|\n\
      \\n\
      \@docs a\n\
      \\n\
      \-}\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \"
    ),
    ( "single line documented exposing with single line block comment",
      "module X exposing (a {- b -})\n\
      \\n\
      \{-|\n\
      \\n\
      \@docs a\n\
      \\n\
      \-}\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \",
      "module X exposing (a {- b -})\n\
      \\n\
      \{-|\n\
      \\n\
      \@docs a\n\
      \\n\
      \-}\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \"
    ),
    ( "single documented export with two single line block comments",
      "module X exposing (a {- b -} {- c -})\n\
      \\n\
      \{-|\n\
      \\n\
      \@docs a\n\
      \\n\
      \-}\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \",
      "module X exposing (a {- b -} {- c -})\n\
      \\n\
      \{-|\n\
      \\n\
      \@docs a\n\
      \\n\
      \-}\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \"
    ),
    ( "documented export row with line comment and undocumented export",
      "module X exposing\n\
      \    ( a, b --\n\
      \    , c\n\
      \    )\n\
      \\n\
      \{-|\n\
      \\n\
      \@docs a, b\n\
      \\n\
      \-}\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \",
      "module X exposing\n\
      \    ( a, b\n\
      \    , c\n\
      \      --\n\
      \    )\n\
      \\n\
      \{-|\n\
      \\n\
      \@docs a, b\n\
      \\n\
      \-}\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \"
    ),
    ( "line comment after multi item documented exposing after single",
      "module X exposing\n\
      \    ( a\n\
      \    , b, c\n\
      \    --\n\
      \    )\n\
      \\n\
      \{-|\n\
      \\n\
      \@docs a\n\
      \\n\
      \@docs b, c\n\
      \\n\
      \-}\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \",
      "module X exposing\n\
      \    ( a\n\
      \    , b, c\n\
      \    --\n\
      \    )\n\
      \\n\
      \{-|\n\
      \\n\
      \@docs a\n\
      \\n\
      \@docs b, c\n\
      \\n\
      \-}\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \"
    ),
    ( "documented export followed by two line comments",
      "module X exposing\n\
      \    ( x\n\
      \    --\n\
      \      --\n\
      \    )\n\
      \\n\
      \{-|\n\
      \\n\
      \@docs x\n\
      \\n\
      \-}\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \",
      "module X exposing\n\
      \    ( x\n\
      \    --\n\
      \    --\n\
      \    )\n\
      \\n\
      \{-|\n\
      \\n\
      \@docs x\n\
      \\n\
      \-}\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \"
    ),
    ( "two line comments in between undocumented exports",
      "module X exposing\n\
      \    ( a\n\
      \      --\n\
      \      --\n\
      \    , b\n\
      \    )\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \",
      "module X exposing\n\
      \    ( a\n\
      \      --\n\
      \      --\n\
      \    , b\n\
      \    )\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \"
    ),
    ( "documented exports interspersed with line comments",
      "module X exposing\n\
      \    ( a\n\
      \    --\n\
      \    , b\n\
      \    --\n\
      \    )\n\
      \\n\
      \{-|\n\
      \\n\
      \@docs a\n\
      \\n\
      \@docs b\n\
      \\n\
      \-}\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \",
      "module X exposing\n\
      \    ( a\n\
      \    , b\n\
      \    --\n\
      \    --\n\
      \    )\n\
      \\n\
      \{-|\n\
      \\n\
      \@docs a\n\
      \\n\
      \@docs b\n\
      \\n\
      \-}\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \"
    )
  ]
