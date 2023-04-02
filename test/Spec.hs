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
    testGroup "Unit tests" $
      property : map oneTest cases

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
    ( "trailing line comment in multi-line record, on same line",
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
    )
  ]
