module Main (main) where

import qualified Memory
import Result
import System.IO (IOMode (ReadWriteMode), withFile)
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
    withFile "temporaryInputFile.elm" ReadWriteMode $ \inFile ->
      withFile "temporaryOutputFile.elm" ReadWriteMode $ \outFile ->
        do
          System.IO.hPutStr inFile before
          System.IO.hSeek inFile System.IO.AbsoluteSeek 0
          result <- Trumat.format inFile outFile memory
          result @?= Ok
          System.IO.hSeek outFile System.IO.AbsoluteSeek 0
          got <- System.IO.hGetContents outFile
          got @?= after

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
