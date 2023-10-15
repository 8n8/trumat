module UpperName (UpperName, parse) where

import SubsequentNameChar (SubsequentNameChar)
import qualified SubsequentNameChar
import UpperChar (UpperChar)
import qualified UpperChar
import qualified ElmChar
import Prelude (Maybe(..), Int, IO, ($), pure, (+), Bool(..))
import ElmChar (ElmChar)
import qualified Prelude
import qualified ElmChars
import ElmChars (ElmChars)

data UpperName
  = UpperName UpperChar [SubsequentNameChar]

parse :: Int -> ElmChars -> IO (Maybe (UpperName, Int))
parse index elmChars =
  do
  firstResult <- ElmChars.get index elmChars
  case firstResult of
    Nothing ->
      pure Nothing
    Just first ->
      case UpperChar.parse first of
        Nothing ->
          pure Nothing
        Just firstChar ->
          do
          subsequentCharsResult <- parseSubsequentChars (index + 1) elmChars
          case subsequentCharsResult of
            Nothing ->
              pure Nothing
            Just (subsequentChars, newIndex) ->
              pure $ Just $ (UpperName firstChar subsequentChars, newIndex)

parseSubsequentChars :: Int -> ElmChars -> IO (Maybe ([SubsequentNameChar], Int))
parseSubsequentChars index elmChars =
  parseSubsequentCharsHelp index elmChars []

parseSubsequentCharsHelp
  :: Int
  -> ElmChars
  -> [SubsequentNameChar]
  -> IO (Maybe ([SubsequentNameChar], Int))
parseSubsequentCharsHelp index elmChars accumulated =
  do
  result <- ElmChars.get index elmChars
  case result of
    Nothing ->
      pure (Just (Prelude.reverse accumulated, index))
    Just elmChar ->
      case SubsequentNameChar.parse elmChar of
        Nothing ->
          if isAfterNameChar elmChar
            then pure (Just (Prelude.reverse accumulated, index))
            else pure Nothing
        Just lowerChar ->
          parseSubsequentCharsHelp
            (index + 1)
            elmChars
            (lowerChar : accumulated)


isAfterNameChar :: ElmChar -> Bool
isAfterNameChar char =
  case char of
    ElmChar.Space -> True
    ElmChar.A_ -> False
    ElmChar.OtherNonSpace -> False
    ElmChar.CloseCurly -> True
    ElmChar.Pipe -> True
    ElmChar.OpenCurly -> True
    ElmChar.Z_ -> False
    ElmChar.Y_ -> False
    ElmChar.X_ -> False
    ElmChar.W_ -> False
    ElmChar.V_ -> False
    ElmChar.U_ -> False
    ElmChar.T_ -> False
    ElmChar.S_ -> False
    ElmChar.R_ -> False
    ElmChar.Q_ -> False
    ElmChar.P_ -> False
    ElmChar.O_ -> False
    ElmChar.N_ -> False
    ElmChar.M_ -> False
    ElmChar.L_ -> False
    ElmChar.K_ -> False
    ElmChar.I_ -> False
    ElmChar.J_ -> False
    ElmChar.H_ -> False
    ElmChar.G_ -> False
    ElmChar.F_ -> False
    ElmChar.E_ -> False
    ElmChar.D_ -> False
    ElmChar.C_ -> False
    ElmChar.B_ -> False
    ElmChar.Backtick -> False
    ElmChar.Underscore -> False
    ElmChar.Power -> True
    ElmChar.CloseBracket -> True
    ElmChar.Backslash -> False
    ElmChar.OpenBracket -> True
    ElmChar.Z' -> False
    ElmChar.Y' -> False
    ElmChar.X' -> False
    ElmChar.W' -> False
    ElmChar.V' -> False
    ElmChar.U' -> False
    ElmChar.T' -> False
    ElmChar.S' -> False
    ElmChar.R' -> False
    ElmChar.Q' -> False
    ElmChar.P' -> False
    ElmChar.O' -> False
    ElmChar.N' -> False
    ElmChar.M' -> False
    ElmChar.L' -> False
    ElmChar.K' -> False
    ElmChar.J' -> False
    ElmChar.I' -> False
    ElmChar.H' -> False
    ElmChar.G' -> False
    ElmChar.F' -> False
    ElmChar.E' -> False
    ElmChar.D' -> False
    ElmChar.C' -> False
    ElmChar.B' -> False
    ElmChar.A' -> False
    ElmChar.GreaterThan -> True
    ElmChar.Equals -> True
    ElmChar.LessThan -> True
    ElmChar.Colon -> True
    ElmChar.Nine -> False
    ElmChar.Eight -> False
    ElmChar.Seven -> False
    ElmChar.Six -> False
    ElmChar.Five -> False
    ElmChar.Four -> False
    ElmChar.Three -> False
    ElmChar.Two -> False
    ElmChar.One -> False
    ElmChar.Zero -> False
    ElmChar.ForwardSlash -> True
    ElmChar.Dot -> True
    ElmChar.Hyphen -> True
    ElmChar.Comma -> True
    ElmChar.Plus -> True
    ElmChar.Asterisk -> True
    ElmChar.CloseParentheses -> True
    ElmChar.OpenParentheses -> True
    ElmChar.SingleQuote -> True
    ElmChar.DoubleQuote -> True
    ElmChar.Newline -> True
