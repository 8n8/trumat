module Trumat (format) where

import Array8 (Array8)
import Ast (Ast)

format :: Array8 -> Ast -> IO (Either String ())
format _ _ =
  return $ Right ()
