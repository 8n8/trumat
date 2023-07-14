module Main (main) where

import Test.Tasty
import Test.Tasty.HUnit
import qualified System.IO

main :: IO ()
main =
    defaultMain $ testGroup "Unit tests" $ map oneTest cases

oneTest :: (String, String, String) -> TestTree
oneTest (name, before, after) =
  testCase name $ do
    memory <- Memory.empty
    (_, inFile) <- System.IO.openTempFile "." "testInput.elm"
    System.IO.hPutStr inFile before
    System.IO.hSeek inFile System.IO.AbsoluteSeek 0
    (_, outFile) <- System.IO.openTempFile "." "testOutput.elm"
    result <- Trumat.format inFile outFile memory
    result @?= Just ()
    System.IO.hSeek outFile System.IO.AbsoluteSeek 0
    got <- System.IO.hGetContents outFile
    got @?= after

cases :: [(String, String, String)]
cases =
    [ ( "hello world formatted"
      , "module X exposing (x)\n\
        \\n\
        \\n\
        \x =\n\
        \    0\n\
        \"
      , "module X exposing (x)\n\
        \\n\
        \\n\
        \x =\n\
        \    0\n\
        \"
      )
    ]
