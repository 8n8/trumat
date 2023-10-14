import Bytes (Bytes)
import qualified Bytes
import qualified Data.Char
import Data.Word (Word8)
import qualified Memory
import qualified Trumat
import Prelude
  ( Bool,
    Char,
    Eq,
    IO,
    Int,
    Maybe (..),
    Show,
    String,
    fmap,
    pure,
    show,
    ($),
    (&&),
    (+),
    (/=),
    (<),
    (<>),
    (==),
  )
import qualified Prelude

main :: IO ()
main =
  do
    results <- Prelude.mapM oneTest cases
    let numFailed :: Int
        numFailed =
          Prelude.sum $ fmap (\result -> if result then 0 else 1) results
        numPassed :: Int
        numPassed =
          Prelude.sum $ fmap (\result -> if result then 1 else 0) results
    Prelude.putStr "\n"
    if numFailed == 0
      then do
        Prelude.putStr "All "
        Prelude.putStr $ show numPassed
        Prelude.putStr " tests passed!\n"
      else do
        Prelude.putStr $ show numPassed
        Prelude.putStr " passed\n"
        Prelude.putStr $ show numFailed
        Prelude.putStr " failed\n"

oneTest :: (String, String, String) -> IO Bool
oneTest (description, input, expected) =
  do
    memory <- Memory.malloc
    inBytes <- Bytes.malloc
    outBytes <- Bytes.malloc

    writeResult <- writeInput input inBytes

    result <- Trumat.format memory inBytes outBytes
    output <- readOutput outBytes

    let passed =
          writeResult == Just ()
            && result == Trumat.Ok
            && output == expected

    Prelude.putStr (if passed then "PASS" else "FAIL")
    Prelude.putStr ": "
    Prelude.putStr description
    Prelude.putStr "\n"

    expectEqual writeResult (Just ())
    expectEqual result Trumat.Ok
    expectEqual output expected

    pure passed

expectEqual :: (Eq a, Show a) => a -> a -> IO ()
expectEqual expected got =
  do
    if expected /= got
      then do
        Prelude.putStr "\n"
        putIndented "EXPECTED:\n\n"
        putIndented (show expected)
        Prelude.putStr "\n\n"
        putIndented "GOT:\n\n"
        putIndented (show got)
        Prelude.putStr "\n\n"
      else pure ()

putIndented :: String -> IO ()
putIndented message =
  Prelude.putStr $ "    " <> message

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
    then Just (Prelude.fromIntegral (Data.Char.ord char))
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
          (Data.Char.chr (Prelude.fromIntegral word) : accumulated)
          (index + 1)
      Nothing ->
        pure $ Prelude.reverse accumulated

cases :: [(String, String, String)]
cases =
  [ ( "hello world with different name",
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
    ),
    ( "hello world",
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
