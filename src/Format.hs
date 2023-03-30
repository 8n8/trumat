module Format (format) where

import qualified Array
import Array (Array)
import Data.Word (Word8)
import Ast (Ast)

format :: Array Word8 -> Ast -> Array Word8 -> IO (Either String ())
format unformatted ast formatted =
    do
    parsed <- parse unformatted ast
    case parsed of
        Left err ->
            return $ Left err

        Right () ->
            print ast unformatted formatted

parse :: Array Word8 -> Ast -> IO (Either String ())
parse unformatted ast =
    do
    parseResult <- parseHelp unformatted ast 0
    case parseResult of
        Right _ ->
            return $ Right ()

        Left err ->
            return $ Left err

data Result
    = Success Int
    | Failure String
    | None

instance Monad Result where
    return x = Success x
    Result >>= f = 

parseHelp unformatted ast i =
    do
    result <- parseTopLevel unformatted ast i
    case result of
        Success j ->
            do

    if j <= i then
        return j
