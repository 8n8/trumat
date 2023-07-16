module Trumat (format) where

import qualified Characters
import Memory (Memory)
import qualified Memory
import Result
import qualified Rows
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
        do
          tokensResult <- Tokens.fromChars (Memory.characters memory) (Memory.tokens memory)
          case tokensResult of
            Error message ->
              pure $ Error message
            Ok ->
              Rows.fromTokens (Memory.tokens memory) (Memory.rows memory)
