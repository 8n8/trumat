module Main (main) where

import qualified Data.ByteString
import qualified Data.Text
import qualified Data.Text.Encoding
import qualified Hedgehog
import qualified Hedgehog.Gen
import qualified Hedgehog.Range
import qualified Test.Tasty
import qualified Test.Tasty.HUnit
import qualified Test.Tasty.Hedgehog
import qualified Trumat

main :: IO ()
main =
  Test.Tasty.defaultMain $
    Test.Tasty.testGroup "Tests" $
      [ unitTests,
        testExpressionProperty
      ]

testExpressionProperty :: Test.Tasty.TestTree
testExpressionProperty =
  Test.Tasty.Hedgehog.testProperty "expression test" $
    Hedgehog.property $
      do
        (unformatted, formatted) <- Hedgehog.forAll generateModule
        Trumat.trumat unformatted Hedgehog.=== Right formatted

generateModule :: Hedgehog.Gen (Data.ByteString.ByteString, Data.ByteString.ByteString)
generateModule =
  do
    (unformatted, formatted) <- generateExpression
    return $
      ( mconcat
          [ "module X exposing (x)\n\
            \\n\
            \\n\
            \x =\n\
            \    ",
            unformatted,
            "\n"
          ],
        mconcat
          [ "module X exposing (x)\n\
            \\n\
            \\n\
            \x =\n\
            \    ",
            formatted,
            "\n"
          ]
      )

generateExpression :: Hedgehog.Gen (Data.ByteString.ByteString, Data.ByteString.ByteString)
generateExpression =
  Hedgehog.Gen.choice [generateIntLiteral, generateSingleLineList]

generateSingleLineList :: Hedgehog.Gen (Data.ByteString.ByteString, Data.ByteString.ByteString)
generateSingleLineList =
  do
    items <- Hedgehog.Gen.list (Hedgehog.Range.constant 0 3) generateIntLiteral
    let unformatted = map snd items
    let formatted = map fst items
    if null items
      then return ("[ ]", "[]")
      else
        return $
          ( mconcat
              [ "[",
                Data.ByteString.intercalate "," unformatted,
                "]"
              ],
            mconcat
              [ "[ ",
                Data.ByteString.intercalate ", " formatted,
                " ]"
              ]
          )

generateIntLiteral :: Hedgehog.Gen (Data.ByteString.ByteString, Data.ByteString.ByteString)
generateIntLiteral =
  do
    int <- Hedgehog.Gen.integral (Hedgehog.Range.constant 0 100)
    let str = show (int :: Int)
    let txt = Data.Text.pack str
    let bytes = Data.Text.Encoding.encodeUtf8 txt
    return (bytes, bytes)

unitTests :: Test.Tasty.TestTree
unitTests =
  Test.Tasty.testGroup "Unit tests" $
    Prelude.map unchangedTest unchangedCases
      ++ Prelude.map unformattedTest unformattedCases

unformattedTest :: (String, Data.ByteString.ByteString, Data.ByteString.ByteString) -> Test.Tasty.TestTree
unformattedTest (name, input, expected) =
  Test.Tasty.HUnit.testCase name $
    Trumat.trumat input Test.Tasty.HUnit.@?= Right expected

unchangedTest :: (String, Data.ByteString.ByteString) -> Test.Tasty.TestTree
unchangedTest (name, input) =
  Test.Tasty.HUnit.testCase name $
    Trumat.trumat input Test.Tasty.HUnit.@?= Right input

unchangedCases :: [(String, Data.ByteString.ByteString)]
unchangedCases =
  [ ( "hello world formatted",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \"
    ),
    ( "hello world formatted",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    [ 0 ]\n\
      \"
    )
  ]

unformattedCases :: [(String, Data.ByteString.ByteString, Data.ByteString.ByteString)]
unformattedCases =
  [ ( "empty list with extra space",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    [ ]\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    []\n\
      \"
    ),
    ( "empty list with extra space",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    [ ]\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    []\n\
      \"
    ),
    ( "singleton list with no spaces",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    [0]\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    [ 0 ]\n\
      \"
    )
  ]
