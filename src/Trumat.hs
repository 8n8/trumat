module Trumat (trumat) where

import Array8 (Array8)
import Ast (Ast)

trumat :: Ast -> Array8 -> IO (Either String ())
trumat _ _ =
    return (Right ())
