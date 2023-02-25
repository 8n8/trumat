module Tokens (Tokens, parse, getById) where

import Data.IntMap.Strict
import Data.IntSet as IntSet
import Data.IntMap as IntMap
import Data.Text (Text)
import Data.Void (Void)
import Text.Megaparsec (Parsec, some)
import qualified Token
import Prelude (Int, foldr, (+), fst, ($), return, Maybe(..))

type Parser = Parsec Void Text

getById :: Int -> Tokens -> Maybe Token.Token
getById id tokens =
    if IntSet.member id (module_ tokens) then
        Just Token.Module

    else
    if IntSet.member id (equals tokens) then
        Just Token.Equals

    else
    if IntSet.member id (exposing_ tokens) then
        Just Token.Exposing

    else
    if IntSet.member id (openParenthesis tokens) then
        Just Token.OpenParenthesis

    else
    if IntSet.member id (closeParenthesis tokens) then
        Just Token.CloseParenthesis

    else
    if IntSet.member id (space tokens) then
        Just Token.Space

    else
    if IntSet.member id (newline tokens) then
        Just Token.Newline

    else
    case IntMap.lookup id (verbatim tokens) of
        Nothing ->
            Nothing

        Just text ->
            Just (Token.Verbatim text)

data Tokens
    = Tokens 
        { module_ :: IntSet
        , verbatim :: IntMap Text
        , equals :: IntSet
        , exposing_ :: IntSet
        , openParenthesis :: IntSet
        , closeParenthesis :: IntSet
        , space :: IntSet
        , newline :: IntSet
        }

empty :: Tokens
empty =
    Tokens
        { module_ = IntSet.empty
        , verbatim = IntMap.empty
        , equals = IntSet.empty
        , exposing_ = IntSet.empty
        , openParenthesis = IntSet.empty
        , closeParenthesis = IntSet.empty
        , space = IntSet.empty
        , newline = IntSet.empty
        }

parse :: Parser Tokens
parse =
    do
    items <- some Token.parse
    return $ fst $ Prelude.foldr parseHelp (Tokens.empty, 0) items

parseHelp :: Token.Token -> (Tokens, Int) -> (Tokens, Int)
parseHelp token (tokens, id) =
    case token of
    Token.Verbatim text ->
        ( tokens { verbatim = IntMap.insert id text (verbatim tokens) }
        , id + 1
        )

    Token.Newline ->
        ( tokens { newline = IntSet.insert id (newline tokens) }, id + 1)

    Token.Module ->
        ( tokens { module_ = IntSet.insert id (module_ tokens) }, id + 1)

    Token.Equals ->
        ( tokens { equals = IntSet.insert id (equals tokens) }, id + 1)

    Token.OpenParenthesis ->
        ( tokens { openParenthesis = IntSet.insert id (openParenthesis tokens) }, id + 1)

    Token.CloseParenthesis ->
        ( tokens { closeParenthesis = IntSet.insert id (closeParenthesis tokens) }, id + 1)

    Token.Exposing ->
        ( tokens { exposing_ = IntSet.insert id (exposing_ tokens) }, id + 1)

    Token.Space ->
        ( tokens { space = IntSet.insert id (space tokens) }, id + 1)
