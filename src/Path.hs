module Path (Path, fromString, toString) where

import Prelude (FilePath, Maybe (Just, Nothing), reverse)

newtype Path
  = Path FilePath

fromString :: FilePath -> Maybe Path
fromString raw =
  case reverse raw of
    'm' : 'l' : 'e' : '.' : _ ->
      Just (Path raw)
    _ ->
      Nothing

toString :: Path -> FilePath
toString (Path path) =
  path
