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
import qualified Effect

main :: IO ()
main =
  Effect.readPureWith Trumat.initModel Trumat.initCmd Trumat.update
