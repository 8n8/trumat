module Token (Token (..), toString, parse) where

import IntLiteral (IntLiteral)
import qualified IntLiteral
import qualified Name
import Data.Text (Text)
import ElmChar (ElmChar)
import qualified ElmChar
import NonEmpty (NonEmpty)
import qualified Uppercase
import qualified Lowercase
import qualified Digit
import Digit (Digit)

data Token
  = Module
  | Name Name.First [Name.Subsequent]
  | Exposing
  | OpenParenthesis
  | CloseParenthesis
  | Newline
  | Space
  | Equals
  | IntLiteral IntLiteral

data State
    = Init
    | ParsingName Name.First [Name.Subsequent]
    | ParsingIntLiteral Digit [Digit]
    | Finished [Token]


parse :: ElmChar -> State -> State
parse raw state =
    case (raw, state) of
        (ElmChar.Lowercase lowercase, Init) ->
            ParsingName (Name.LowercaseF lowercase) []

        ( ElmChar.Alphabet char
        , ParsingName
            Alphabet.Cm
            [ Alphabet.Co
            , Alphabet.Cd
            , Alphabet.Cu
            , Alphabet.Cl
            , Alphabet.Ce
            ]) ->
            case char of
                Alphabet.Ca
            if isAfterKeywordChar char then
                case parse char Init of
                    Init ->
                        Failed "sub parser didn't move on from init"

                    ParsingName _ _ ->
                        Failed "sub parser didn't finish parsing name"

                    ParsingIntLiteral _ _ ->
                        Failed "sub parser didn't finish parsing int literal"

                    Finished tokens ->
                        Finished (Moddule : tokens)

            else
              case char of
                
                

        (ElmChar.Space, Init) ->
            Finished Space

        (ElmChar.Uppercase uppercase, Init) ->
            ParsingName (Name.UppercaseF uppercase) []

        (ElmChar.Digit digit, Init) ->
            ParsingIntLiteral digit []

        (ElmChar.Newline, Init) ->
            Finished Newline

        (ElmChar.OpenParenthesis, Init) ->
            Finished OpenParenthesis

        (ElmChar.CloseParenthesis, Init) ->
            Finished CloseParenthesis

        (ElmChar.Equals, Init) ->
            Finished Equals


toString :: Token -> Text
toString token =
    case token of
        Module ->
            "module"

        Name first subsequent ->
            Name.toString first subsequent

        Exposing ->
            "exposing"

        OpenParenthesis ->
            "("

        CloseParenthesis ->
            ")"

        Newline ->
            "\n"

        Space ->
            " "

        Equals ->
            "="

        IntLiteral intLiteral ->
            IntLiteral.toString intLiteral
