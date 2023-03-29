module Format (format) where

import qualified Array
import Array (Array)
import Data.Word (Word8)
import Ast (Ast)

format :: Array Word8 -> Ast -> Array Word8 -> IO (Either String ())
format unformatted _ formatted =
    fmap Right (formatHelp unformatted formatted 0)


formatHelp :: Array Word8 -> Array Word8 -> Int -> IO ()
formatHelp unformatted formatted index =
    do
    getResult <- Array.get index unformatted
    case getResult of
        Nothing ->
            return ()

        Just byte ->
            do
            appendResult <- Array.append byte formatted
            case appendResult of
                Nothing ->
                    return ()

                Just () ->
                    formatHelp unformatted formatted (index + 1)
