module ElmChars (ElmChar, parse) where

import Prelude
import Data.Text (Text)
import qualified Data.Text as Text

data ElmChar
    = Ml
    | Ol
    | Dl
    | Ul
    | Ll
    | El
    | Space
    | Xu
    | Xl
    | Pl
    | Sl
    | Il
    | Nl
    | Gl
    | OpenParentheses
    | CloseParentheses
    | Newline
    | Equals
    | D0


parse :: Text -> Either String [ElmChar]
parse raw =
    parseHelp raw []


parseHelp :: Text -> [ElmChar] -> Either String [ElmChar]
parseHelp raw accumulator =
    case Text.uncons raw of
        Nothing ->
            Right (reverse accumulator)

        Just (first, remainder) ->
            case parseOne first of
                Left err ->
                    Left err

                Right elmChar ->
                    parseHelp remainder (elmChar : accumulator)


parseOne :: Char -> Either String ElmChar
parseOne char =
    case char of
    'm' ->
        Right Ml

    'o' ->
        Right Ol

    'd' ->
        Right Dl

    'u' ->
        Right Ul

    'l' ->
        Right Ll

    'e' ->
        Right El

    ' ' ->
        Right Space

    'X' ->
        Right Xu

    'x' ->
        Right Xl

    'p' ->
        Right Pl

    's' ->
        Right Sl

    'i' ->
        Right Il

    'n' ->
        Right Nl

    'g' ->
        Right Gl

    '(' ->
        Right OpenParentheses

    ')' ->
        Right CloseParentheses

    '\n' ->
        Right Newline

    '=' ->
        Right Equals

    '0' ->
        Right D0

    _ ->
        Left $ "invalid character: " <> [char]
        
