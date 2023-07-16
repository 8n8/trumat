module Main (main) where

import qualified Memory
import Result
import System.Directory (removeFile)
import qualified System.IO
import Test.Tasty
import Test.Tasty.HUnit
import qualified Trumat

main :: IO ()
main =
  defaultMain $ testGroup "Unit tests" $ map oneTest cases

oneTest :: (String, String, String) -> TestTree
oneTest (name, before, after) =
  testCase name $ do
    memory <- Memory.empty
    (inPath, inFile) <- System.IO.openTempFile "." "testInput.elm"
    System.IO.hPutStr inFile before
    System.IO.hSeek inFile System.IO.AbsoluteSeek 0
    (outPath, outFile) <- System.IO.openTempFile "." "testOutput.elm"
    result <- Trumat.format inFile outFile memory
    System.IO.hClose inFile
    removeFile inPath
    if result /= Ok
      then do
        result @?= Ok
        System.IO.hClose outFile
        removeFile outPath
      else do
        System.IO.hSeek outFile System.IO.AbsoluteSeek 0
        got <- System.IO.hGetContents outFile
        got @?= after
        System.IO.hClose outFile
        removeFile outPath

cases :: [(String, String, String)]
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
