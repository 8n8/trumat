module Ast (Ast, new) where

import Expression (Expression)
import qualified Expression
import HasNewline (HasNewline)
import qualified HasNewline
import List (List)
import qualified List
import TopBind (TopBind)
import qualified TopBind
import Bind (Bind)
import qualified Bind
import Export (Export)
import qualified Export
import ModuleName (ModuleName)
import qualified ModuleName
import Verbatim (Verbatim)
import qualified Verbatim

data Ast
    = Ast
        Verbatim
        ModuleName
        Export
        Bind
        TopBind
        List
        HasNewline
        Expression

new :: IO Ast
new =
    do
    verbatim <- Verbatim.new
    moduleName <- ModuleName.new
    export <- Export.new
    bind <- Bind.new
    topBind <- TopBind.new
    list <- List.new
    hasNewline <- HasNewline.new
    expression <- Expression.new
    return $ Ast verbatim moduleName export bind topBind list hasNewline expression
