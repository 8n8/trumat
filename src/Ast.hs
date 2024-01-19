module Ast (Ast, parse, write) where

import Data.Attoparsec.ByteString.Char8 (Parser)
import qualified Data.Attoparsec.ByteString.Char8
import Data.ByteString (ByteString)
import Data.Function ((&))
import ModuleDeclaration (ModuleDeclaration)
import qualified ModuleDeclaration
import TopLevel (TopLevel)
import qualified TopLevel

data Ast
  = Ast (Maybe ModuleDeclaration) TopLevel [TopLevel]

write :: Ast -> ByteString
write (Ast maybeDeclaration topLevel _) =
  [ case maybeDeclaration of
      Nothing ->
        ""
      Just declaration ->
        ModuleDeclaration.write declaration <> "\n\n\n",
    TopLevel.write topLevel,
    "\n"
  ]
    & mconcat

parse :: Parser Ast
parse =
  do
    moduleDeclaration <-
      Data.Attoparsec.ByteString.Char8.choice
        [ fmap Just ModuleDeclaration.parse,
          pure Nothing
        ]
    _ <- Data.Attoparsec.ByteString.Char8.takeWhile (\ch -> ch == '\n')
    topLevel <- TopLevel.parse
    _ <- Data.Attoparsec.ByteString.Char8.takeWhile (\ch -> ch == '\n')
    pure $ Ast moduleDeclaration topLevel []
