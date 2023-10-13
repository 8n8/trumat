module Main (main) where

import Control.Exception (try)
import Data.ByteString (readFile, writeFile)
import Data.Text.Encoding (decodeUtf8', encodeUtf8)
import Memory
import Path (Path)
import qualified Path
import qualified System.Directory
import System.IO
import qualified Trumat
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

temporaryPath :: FilePath
temporaryPath =
  ".trumatTemp"

main :: IO ()
main =
  do
    memory <- Memory.malloc
    formatPath memory "."

formatPath :: Memory -> FilePath -> IO ()
formatPath memory path =
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
              formatFile memory elmPath
      Right contents ->
        mapM_ (formatPath memory) $ map (\dir -> path <> "/" <> dir) contents

listDirectory :: FilePath -> IO (Either IOError [FilePath])
listDirectory path =
  try $ System.Directory.listDirectory path

formatFile :: Memory -> Path -> IO ()
formatFile memory path =
  do
    result <-
      System.IO.withFile (Path.toString path) System.IO.ReadMode $ \inHandle ->
        System.IO.withFile temporaryPath System.IO.WriteMode $ \outHandle ->
          Trumat.format memory inHandle outHandle

    case result of
      Trumat.Ok ->
        System.Directory.renameFile temporaryPath (Path.toString path)
      Trumat.Error errorMessage ->
        putStrLn $ "not a valid Elm file: " <> errorMessage
