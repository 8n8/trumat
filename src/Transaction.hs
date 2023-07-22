module Transaction (Transaction(..)) where

import Data.Word (Word32)

data Transaction
    = ModuleName Word32
    | IsExport Word32
    | TopBindLeft Word32
    | TopBindRight Word32
    | ParameterChild Word32
    | UnitValue
    | Token Word32
