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
    ( "at docs",
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
    ( "multiline string in function argument in function argument",
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
    ( "multiline string in function argument in function argument",
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
    )
  ]
