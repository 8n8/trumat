module Main (main) where

import qualified Control.Exception
import Data.ByteString (ByteString)
import qualified Data.ByteString
import qualified Data.Text.Encoding
import ElmPath (ElmPath)
import qualified ElmPath
import qualified System.Directory
import qualified System.Environment
import qualified Trumat

main :: IO ()
main =
  do
    args <- System.Environment.getArgs
    case parseArgs args of
      Nothing ->
        putStr usage
      Just path ->
        formatPath path

parseArgs :: [String] -> Maybe FilePath
parseArgs args =
  case args of
    ["--overwrite", "."] ->
      Nothing
    ["--overwrite", ".."] ->
      Nothing
    ["--overwrite", path] ->
      Just path
    _ ->
      Nothing

usage :: String
usage =
  "Trumat is a tool for formatting Elm files.\n\
  \\n\
  \It expects two arguments:\n\
  \1. --overwrite to confirm it is OK to recursivly overwrite the Elm files\n\
  \2. the path to the Elm file or directory of Elm files to format\n\
  \"

formatPath :: FilePath -> IO ()
formatPath path =
  do
    eitherContents <- Control.Exception.try (System.Directory.listDirectory path) :: (IO (Either IOError [String]))
    case eitherContents of
      Left _ ->
        case ElmPath.parse path of
          Nothing ->
            pure ()
          Just elmPath ->
            formatElmPath elmPath
      Right subPaths ->
        mapM_ (\subPath -> formatPath (path <> "/" <> subPath)) subPaths

formatElmPath :: ElmPath -> IO ()
formatElmPath path =
  do
    eitherBytes <- (Control.Exception.try (Data.ByteString.readFile (ElmPath.toString path))) :: IO (Either IOError ByteString)
    case eitherBytes of
      Left err ->
        putStrLn $
          mconcat
            [ "Error reading the file with path: ",
              ElmPath.toString path,
              "\n    ",
              show err
            ]
      Right bytes ->
        case Data.Text.Encoding.decodeUtf8' bytes of
          Left err ->
            putStrLn $
              mconcat
                [ "Error decoding the file with path: ",
                  ElmPath.toString path,
                  "\n    ",
                  show err
                ]
          Right text ->
            case Trumat.format text of
              Left err ->
                putStrLn $
                  mconcat
                    [ "Error formatting the file with path: ",
                      ElmPath.toString path,
                      "\n    ",
                      show err
                    ]
              Right formattedText ->
                Data.ByteString.writeFile (ElmPath.toString path) (Data.Text.Encoding.encodeUtf8 formattedText)
