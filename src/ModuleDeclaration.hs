module ModuleDeclaration (ModuleDeclaration, parse, write) where

import Data.Attoparsec.ByteString.Char8 (Parser)
import qualified Data.Attoparsec.ByteString.Char8 as Attoparsec
import Data.ByteString (ByteString)

data ModuleDeclaration
  = ModuleDeclaration

parse :: Parser ModuleDeclaration
parse =
  do
    _ <- Attoparsec.string "module X exposing (x)"
    pure ModuleDeclaration

write :: ModuleDeclaration -> ByteString
write _ =
  "module X exposing (x)"
