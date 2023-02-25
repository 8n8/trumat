module Token (Token(..), parse) where

import Data.Text (Text)
import Text.Megaparsec (Parsec, choice, lookAhead, chunk, takeWhile1P)
import Data.Void (Void)
import Text.Megaparsec.Char (char)

data Token
    = Module
    | Equals
    | OpenParenthesis
    | CloseParenthesis
    | Exposing
    | Space
    | Newline
    | Verbatim Text

type Parser = Parsec Void Text

parse :: Parser Token
parse =
    choice
        [ parseMulti "module" Module
        , parseMulti "exposing" Exposing
        , parseSingle '(' OpenParenthesis
        , parseSingle ')' CloseParenthesis
        , parseSingle '=' Equals
        , parseSingle ' ' Space
        , parseSingle '\n' Newline
        , parseVerbatim
        ]

parseVerbatim :: Parser Token
parseVerbatim =
    do
    chars <- takeWhile1P Nothing (\ch -> ch `elem` ("0123456789" :: String))
    return $ Verbatim chars

parseMulti :: Text -> Token -> Parser Token
parseMulti text symbol =
    do
    _ <- chunk text
    _ <- lookAhead $ choice [char ' ', char '\n']
    return symbol

parseSingle :: Char -> Token -> Parser Token
parseSingle ch symbol =
    do
    _ <- char ch
    return symbol
