module Main (main) where

import Test.Tasty
import Test.Tasty.HUnit


main :: IO ()
main =
    defaultMain tests


tests :: TestTree
tests = testGroup "Tests"
    [ testCase "2+2=4" $
        2+2 @?= 4
    ]
