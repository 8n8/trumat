module Trumat (format) where

import qualified Characters
import Memory (Memory)
import qualified Memory
import Result
import System.IO (Handle)
import qualified Tokens

format :: Handle -> Handle -> Memory -> IO Result
format inFile _ memory =
  do
    readInResult <- Characters.fromFile inFile (Memory.characters memory)
    case readInResult of
      Error message ->
        return $ Error message
      Ok ->
        Tokens.fromChars (Memory.characters memory) (Memory.tokens memory)
