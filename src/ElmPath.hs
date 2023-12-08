module ElmPath (ElmPath, parse, toString) where

newtype ElmPath
  = ElmPath String

toString :: ElmPath -> String
toString (ElmPath root) =
  root <> ".elm"

parse :: FilePath -> Maybe ElmPath
parse raw =
  case reverse raw of
    'm' : 'l' : 'e' : '.' : rest ->
      Just (ElmPath (reverse rest))
    _ ->
      Nothing
