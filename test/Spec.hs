import Bytes (Bytes)
import qualified Bytes
import qualified Data.Char
import Data.Function ((&))
import Data.Text (Text, intercalate, pack)
import Data.Word (Word8)
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

main :: IO ()
main =
  map oneTest (zip cases [0 ..])
    & testGroup "Unit tests"
    & defaultMain

oneTest :: ((String, String, String), Int) -> TestTree
oneTest ((description, input, expected), counter) =
  testCase description $
    do
      memory <- Memory.malloc
      inBytes <- Bytes.malloc
      outBytes <- Bytes.malloc

      writeResult <- writeInput input inBytes
      writeResult @?= Just ()

      result <- Trumat.format memory inBytes outBytes
      result @?= Trumat.Ok

      output <- readOutput outBytes
      output @?= expected

writeInput :: String -> Bytes -> IO (Maybe ())
writeInput string bytes =
  case string of
    top : remainder ->
      case charToWord8 top of
        Nothing ->
          pure Nothing
        Just word ->
          do
            result <- Bytes.append bytes word
            case result of
              Bytes.Ok ->
                writeInput remainder bytes
              Bytes.NotEnoughSpace ->
                pure Nothing
    [] ->
      pure $ Just ()

charToWord8 :: Char -> Maybe Word8
charToWord8 char =
  if Data.Char.ord char < 256
    then Just (fromIntegral (Data.Char.ord char))
    else Nothing

readOutput :: Bytes -> IO String
readOutput bytes =
  readOutputHelp bytes "" 0

readOutputHelp :: Bytes -> String -> Int -> IO String
readOutputHelp bytes accumulated index =
  do
    result <- Bytes.get index bytes
    case result of
      Just word ->
        readOutputHelp
          bytes
          (Data.Char.chr (fromIntegral word) : accumulated)
          (index + 1)
      Nothing ->
        pure $ reverse accumulated

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
    ),
    ( "hello world with different name",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \y =\n\
      \    0\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \y =\n\
      \    0\n\
      \"
    )
  ]
