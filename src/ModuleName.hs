module ModuleName (ModuleName, new) where

import Data.Word (Word32)
import Data.IORef (IORef, newIORef)

newtype ModuleName
    = ModuleName (IORef (Maybe Word32))


new :: IO ModuleName
new =
    fmap ModuleName (newIORef Nothing)
