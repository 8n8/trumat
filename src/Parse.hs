module Parse (parse) where

import qualified Data.Attoparsec.ByteString as Attoparsec
import Data.ByteString (ByteString)

parse :: Attoparsec.Parser ByteString
parse =
  return ""
