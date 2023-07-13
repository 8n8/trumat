module Main (main) where

import Data.Text (Text)
import qualified Data.Text as Text
import Hedgehog (Gen)
import qualified Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Tasty
import qualified Test.Tasty.HUnit as HUnit
import qualified Test.Tasty.Hedgehog
import Trumat (format)

main :: IO ()
main =
  defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [unitTests, properties]

properties :: TestTree
properties =
  testGroup
    "Property tests"
    [ Test.Tasty.Hedgehog.testProperty "spaces after module keyword" $ Hedgehog.property $ do
        spaces <- Hedgehog.forAll $ Gen.text (Range.linear 0 1000) (pure ' ')
        let input_ :: Text
            input_ =
              mconcat
                [ "module " <> spaces <> "X exposing (x)\n",
                  "\n",
                  "\n",
                  "x =\n",
                  "    0\n"
                ]
            expected_ :: Text
            expected_ =
              "module X exposing (x)\n\
              \\n\
              \\n\
              \x =\n\
              \    0\n\
              \"
        format input_ Hedgehog.=== Right expected_,
      Test.Tasty.Hedgehog.testProperty "random module name" $ Hedgehog.property $ do
        name <- Hedgehog.forAll generateUpperName
        let input_ :: Text
            input_ =
              mconcat
                [ "module " <> name <> " exposing (x)\n",
                  "\n",
                  "\n",
                  "x =\n",
                  "    0\n"
                ]
        format input_ Hedgehog.=== Right input_,
      Test.Tasty.Hedgehog.testProperty "random spaces and newline after module keyword" $ Hedgehog.property $ do
        spaces <- Hedgehog.forAll $ Gen.text (Range.linear 0 100) (Gen.element [' ', '\n'])
        let input_ :: Text
            input_ =
              mconcat
                [ "module " <> spaces <> " " <> "X exposing (x)\n",
                  "\n",
                  "\n",
                  "x =\n",
                  "    0\n"
                ]

            expected_ :: Text
            expected_ =
              "module X exposing (x)\n\
              \\n\
              \\n\
              \x =\n\
              \    0\n\
              \"

        format input_ Hedgehog.=== Right expected_
    ]

generateUpperName :: Gen Text
generateUpperName =
  do
    firstChar <- Gen.upper
    remainder <- Gen.text (Range.linear 0 100) (Gen.element "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789_")
    return (Text.cons firstChar remainder)

unitTests :: TestTree
unitTests =
  testGroup "Unit tests" (map oneTest cases)

oneTest :: Case -> TestTree
oneTest case_ =
  HUnit.testCase (description case_) $
    expected case_ HUnit.@?= format (input case_)

data Case = Case
  { expected :: Either String Text,
    description :: String,
    input :: Text
  }

cases :: [Case]
cases =
  [ Case
      { expected =
          Right
            "module X exposing (x)\n\
            \\n\
            \\n\
            \x =\n\
            \    0\n\
            \",
        description = "hello world formatted",
        input =
          "module X exposing (x)\n\
          \\n\
          \\n\
          \x =\n\
          \    0\n\
          \"
      }
  ]
