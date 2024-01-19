module Main (main) where

import qualified Control.Exception
import Data.ByteString (ByteString)
import qualified Data.ByteString
import Data.Function ((&))
import qualified System.Directory
import qualified System.Environment
import qualified System.Exit
import qualified Trumat

main :: IO ()
main =
  do
    rawArgs <- System.Environment.getArgs
    case argsParse rawArgs of
      Nothing ->
        putStr usage
      Just path ->
        formatPath path

isElmPath :: FilePath -> Bool
isElmPath path =
  case reverse path of
    'm' : 'l' : 'e' : '.' : _ ->
      True
    _ ->
      False

argsParse :: [String] -> Maybe FilePath
argsParse args =
  case args of
    ["--overwrite", path] ->
      Just path
    _ ->
      Nothing

usage :: String
usage =
  [ "expects two arguments:\n",
    "1. \"--overwrite\" to confirm you understand it will recursively overwrite all the Elm files in the path\n",
    "2. path to format\n"
  ]
    & mconcat

listDirectory :: FilePath -> IO (Either IOError [FilePath])
listDirectory path =
  Control.Exception.try $ System.Directory.listDirectory path

readFileBytes :: FilePath -> IO (Either IOError ByteString)
readFileBytes path =
  Control.Exception.try (Data.ByteString.readFile path)

formatElmPath :: FilePath -> IO ()
formatElmPath path =
  do
    eitherContents <- readFileBytes path
    case eitherContents of
      Left err ->
        do
          putStrLn $
            mconcat $
              [ "error reading ",
                path,
                ": ",
                show err
              ]
          System.Exit.exitWith (System.Exit.ExitFailure 1)
      Right contents ->
        case Trumat.format contents of
          Left err ->
            do
              putStrLn $
                mconcat
                  [ "could not format ",
                    path,
                    ": ",
                    err
                  ]
              System.Exit.exitWith (System.Exit.ExitFailure 1)
          Right formatted ->
            do
              putStrLn $ mconcat ["Processing file ", path]
              Data.ByteString.writeFile path formatted

formatPath :: FilePath -> IO ()
formatPath path =
  do
    eitherListing <- listDirectory path
    case eitherListing of
      Left _ ->
        if isElmPath path
          then formatElmPath path
          else pure ()
      Right listing ->
        mapM_ formatPath (map (\right -> path <> "/" <> right) listing)
