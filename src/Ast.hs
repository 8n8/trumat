module Ast (Ast, malloc) where

data Ast
  = Ast

malloc :: IO Ast
malloc =
  return Ast
