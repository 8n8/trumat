module Trumat (trumat) where

import Data.Text (Text)
import qualified Tokens
import Tokens (Token)
import qualified ElmChars
import Prelude
  ( undefined,
    Either (..),
    String,
    reverse,
    ($)
  )

trumat :: Text -> Either String Text
trumat unformatted =
  case ElmChars.parse unformatted of
    Left err ->
      Left err

    Right elmChars ->
      case Tokens.parse elmChars of
        Left err ->
          Left err
        Right tokens ->
          case format Init tokens [] of
            Left err ->
              Left err

            Right formatted ->
              Right $ detokenize formatted


data State
    = Init


format :: State -> [Token] -> [Token] -> Either String [Token]
format oldState unformatted formatted =
    case unformatted of
        [] ->
            Right $ reverse formatted

        top : remainder ->
            case formatOne oldState top of
                Left err ->
                    Left err

                Right (newState, formattedToken) ->
                    format newState remainder (formattedToken : formatted)


formatOne :: State -> Token -> Either String (State, Token)
formatOne state token =
    undefined


detokenize :: [Token] -> Text
detokenize =
    undefined
