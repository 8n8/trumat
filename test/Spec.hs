module Main (main) where

import Data.Text (Text)
import Test.Tasty
import Test.Tasty.HUnit
import Trumat

main :: IO ()
main =
  defaultMain $
    testGroup "Tests" $
      Prelude.map oneTest cases

oneTest :: (String, Text, Text) -> TestTree
oneTest (name, unformatted, expected) =
  testCase name $
    trumat unformatted @?= Right expected

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
