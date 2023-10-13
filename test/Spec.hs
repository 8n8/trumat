import Data.Function ((&))
import Data.Text (Text, intercalate, pack)
import qualified Hedgehog
import qualified Hedgehog.Gen
import qualified Hedgehog.Range
import qualified Memory
import qualified System.IO
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import qualified Test.Tasty.Hedgehog
import qualified Trumat
import Prelude
  ( Either (..),
    IO,
    Int,
    String,
    fmap,
    map,
    mapM,
    mconcat,
    readFile,
    repeat,
    return,
    take,
    writeFile,
    ($),
    (+),
    (<>),
  )

main :: IO ()
main =
  map oneTest cases
    & testGroup "Unit tests"
    & defaultMain

oneTest :: (String, String, String) -> TestTree
oneTest (description, input, expected) =
  testCase description $
    do
      writeFile temporaryTestingInputPath input
      memory <- Memory.malloc
      result <-
        System.IO.withFile temporaryTestingInputPath System.IO.ReadMode $ \inputHandle ->
          System.IO.withFile temporaryTestingOutputPath System.IO.WriteMode $ \outputHandle ->
            Trumat.format memory inputHandle outputHandle

      result @?= Trumat.Ok
      output <- readFile temporaryTestingOutputPath
      output @?= expected

temporaryTestingInputPath :: String
temporaryTestingInputPath =
  ".trumatTemporaryTestingInput"

temporaryTestingOutputPath :: String
temporaryTestingOutputPath =
  ".trumatTemporaryTestingOutput"

cases :: [(String, String, String)]
cases =
  [ ( "hello world",
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
