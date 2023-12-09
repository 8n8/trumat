jmodule Main (main) where

import Data.Text (Text)
import Test.Tasty (TestTree)
import qualified Test.Tasty
import qualified Test.Tasty.HUnit
import qualified Trumat

main :: IO ()
main =
  Test.Tasty.defaultMain tests

tests :: TestTree
tests =
  Test.Tasty.testGroup "Unit tests" (map oneFormattedTest formattedCases)

oneFormattedTest :: (String, Text) -> TestTree
oneFormattedTest (description, formatted) =
  Test.Tasty.HUnit.testCase description $
    Trumat.format formatted Test.Tasty.HUnit.@?= Right formatted

formattedCases :: [(String, Text)]
formattedCases =
  [ ( "hello world",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \"
    )
  ]
