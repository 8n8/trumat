module Trumat (format, Result (..)) where

import Memory (Memory)
import System.IO (Handle)
import qualified System.IO
import Prelude
  ( Eq,
    IO,
    Show,
    String,
    pure,
    undefined,
  )

data Result
  = Ok
  | Error String
  deriving (Eq, Show)

format :: Memory -> Handle -> Handle -> IO Result
format _ _ outHandle =
  do
    System.IO.hPutStr
      outHandle
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \"
    pure Ok
