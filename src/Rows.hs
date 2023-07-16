module Rows (Rows, empty, fromTokens) where

import Array (Array)
import qualified Array
import Data.Word (Word16)
import MaxTokens (maxTokens)
import Result
import qualified TokenTag
import Tokens (Tokens)
import qualified Tokens

newtype Rows
  = Rows (Array Word16)

empty :: IO Rows
empty =
  fmap Rows (Array.empty maxTokens)

fromTokens :: Tokens -> Rows -> IO Result
fromTokens tokens (Rows rows) =
  fromTokensHelp tokens rows 0 0

fromTokensHelp :: Tokens -> Array Word16 -> Int -> Word16 -> IO Result
fromTokensHelp tokens rows index row =
  do
    tokenResult <- Tokens.get index tokens
    case tokenResult of
      Left _ ->
        pure Ok
      Right token ->
        if Tokens.toTag token == TokenTag.Newline
          then do
            rowsResult <- Array.append (row + 1) rows
            case rowsResult of
              Error error' ->
                pure $ Error error'
              Ok ->
                fromTokensHelp tokens rows (index + 1) (row + 1)
          else do
            rowsResult <- Array.append row rows
            case rowsResult of
              Error error' ->
                pure $ Error error'
              Ok ->
                fromTokensHelp tokens rows (index + 1) row
