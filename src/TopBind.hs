module TopBind (TopBind, parse, toString) where

import Expression (Expression)
import qualified Expression

data TopBind
  = TopBind LowerName Expression

parse :: Parser Bind
parse =
  do
  name <- LowerName.parse
  _ <- ManyWhitespace.parse
  _ <- char '='
  _ <- ManyWhitespace.parse
  expression <- Expression.parse
  pure $ TopBind name expression
