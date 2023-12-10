module Ast (Ast, parse, toString) where

import Bind (Bind)

data Ast
  = Ast [TopNode]

data TopNode
  = ModuleDeclaration ModuleDeclaration
  | Bind Bind
