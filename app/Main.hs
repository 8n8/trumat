module Main (main) where

import Control.Exception (try)
import Data.ByteString (readFile, writeFile)
import Data.Text.Encoding (decodeUtf8', encodeUtf8)
import Path (Path)
import qualified Path
import qualified System.Directory
import Trumat (trumat)
import Prelude
  ( Either (Left, Right),
    FilePath,
    IO,
    IOError,
    Maybe (Just, Nothing),
    map,
    mapM_,
    putStrLn,
    return,
    ($),
    (<>),
  )

main :: IO ()
main =
  formatPath "."

formatPath :: FilePath -> IO ()
formatPath path =
  do
    eitherContents <- listDirectory path
    case eitherContents of
      Left _ ->
        case Path.fromString path of
          Nothing ->
            return ()
          Just elmPath ->
            do
            putStrLn path
            formatFile elmPath
      Right contents ->
        mapM_ formatPath $ map (\dir -> path <> "/" <> dir) contents

listDirectory :: FilePath -> IO (Either IOError [FilePath])
listDirectory path =
  try $ System.Directory.listDirectory path

formatFile :: Path -> IO ()
formatFile path =
  do
    bytes <- readFile (Path.toString path)
    case decodeUtf8' bytes of
      Left _ ->
        putStrLn "not a valid Elm file"
      Right text ->
        case trumat text of
          Left err ->
            putStrLn err
          Right formatted ->
            writeFile (Path.toString path) (encodeUtf8 formatted)
