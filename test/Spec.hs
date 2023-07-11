module Main (main) where

import qualified Test.Tasty.HUnit as HUnit
import Test.Tasty
import Data.Text (Text)
import Trumat (format)

main :: IO ()
main =
    defaultMain tests


tests :: TestTree
tests = testGroup "Tests" (map oneTest cases)


oneTest :: Case -> TestTree
oneTest case_ =
    HUnit.testCase (description case_) $
        expected case_ HUnit.@?= format (input case_)


data Case
    = Case
    { expected :: Either String Text
    , description :: String
    , input :: Text
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
