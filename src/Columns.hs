module Columns (Columns, empty, fromTokens, get) where

import Array (Array)
import qualified Array
import Data.Word (Word16)
import MaxTokens (maxTokens)
import Result
import qualified TokenTag
import Tokens (Tokens)
import qualified Tokens

get :: Int -> Columns -> IO (Either String Word16)
get index (Columns columns) =
   Array.get index columns

newtype Columns
  = Columns (Array Word16)

empty :: IO Columns
empty =
  fmap Columns (Array.empty maxTokens)

fromTokens :: Tokens -> Columns -> IO Result
fromTokens tokens (Columns columns) =
  fromTokensHelp tokens columns 0 0

fromTokensHelp :: Tokens -> Array Word16 -> Int -> Word16 -> IO Result
fromTokensHelp tokens columns index column =
  do
    tokenResult <- Tokens.get index tokens
    case tokenResult of
      Left _ ->
        pure Ok
      Right token ->
        do
          columnsResult <- Array.append column columns
          case columnsResult of
            Error error' ->
              pure (Error error')
            Ok ->
              fromTokensHelp
                tokens
                columns
                (index + 1)
                ( if Tokens.toTag token == TokenTag.Newline
                    then 0
                    else column + 1
                )
