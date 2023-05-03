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
    ( "@docs",
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
      \import A as B exposing\n\
      \    ( C\n\
      \    , D\n\
      \    )\n\
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
      \{-|\n\
      \\n\
      \\n\
      \-}\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \"
    ),
    ( "module @docs and freestyle",
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
    )
  ]
