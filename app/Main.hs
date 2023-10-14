module Main (main) where

import Bytes (Bytes)
import qualified Bytes
import Control.Exception (try)
import Memory (Memory)
import qualified Memory
import Path (Path)
import qualified Path
import qualified System.Directory
import System.IO
import qualified Trumat
import Prelude
  ( Either (Left, Right),
    IOError,
    Maybe (Just, Nothing),
    map,
    mapM_,
    return,
    ($),
    (<>),
  )

temporaryPath :: FilePath
temporaryPath =
  ".trumatTemp"

main :: IO ()
main =
  do
    memory <- Memory.malloc
    input <- Bytes.malloc
    output <- Bytes.malloc
    formatPath memory input output "."

formatPath :: Memory -> Bytes -> Bytes -> FilePath -> IO ()
formatPath memory input output path =
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
              formatFile memory input output elmPath
      Right contents ->
        mapM_ (formatPath memory input output) $
          map (\dir -> path <> "/" <> dir) contents

listDirectory :: FilePath -> IO (Either IOError [FilePath])
listDirectory path =
  try $ System.Directory.listDirectory path

formatFile :: Memory -> Bytes -> Bytes -> Path -> IO ()
formatFile memory input output path =
  do
    Bytes.zero input
    Bytes.zero output
    Bytes.readFile (Path.toString path) input
    result <- Trumat.format memory input output
    case result of
      Trumat.Ok ->
        Bytes.writeFile output (Path.toString path)
      Trumat.Error errorMessage ->
        putStrLn $ "not a valid Elm file: " <> errorMessage
