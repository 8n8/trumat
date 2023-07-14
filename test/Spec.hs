module Main (main) where

import Test.Tasty (TestTree, testGroup, defaultMain)
import Test.Tasty.HUnit ((@?=), testCase)
import qualified Trumat

main :: IO ()
main =
  defaultMain tests

tests :: TestTree
tests =
  testGroup
    "Tests"
    [ testCase "2+2=4" $
        2 + 2 @?= 4,
      testCase "hello world formatted" $
        (@?=)
          ( Trumat.format
              "module X exposing (x)\n\
              \\n\
              \\n\
              \x =\n\
              \    0\n\
              \"
          )
          ( Right
              "module X exposing (x)\n\
              \\n\
              \\n\
              \x =\n\
              \    0\n\
              \"
          )
    ]
