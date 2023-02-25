module Ast (Ast, parse) where

import Data.IntSet (IntSet)
import Data.IntMap (IntMap)
import Data.Text (Text)
import Tokens (Tokens)

data Ast
    = Ast
        { verbatims :: IntMap Text 
        , binds :: IntMap Int
        , exposing :: IntSet
        , moduleName :: IntSet
        , topLevelBinds :: IntSet
        , nextId :: Int
        } 


parse :: Tokens -> Either String Ast
parse tokens =
    case parseModuleDeclaration tokens empty of
        Left err ->
            Left err

        Right ast ->
            parseTopLevelBinds tokens ast


parseTopLevelBinds :: Tokens -> Ast -> Either String Ast
parseTopLevelBinds tokens ast =
   let
       topLevelEqualsTokenIds
