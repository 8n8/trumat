module LowerName (LowerName, parse) where

import SubsequentNameChar (SubsequentNameChar)
import FirstLowerNameChar (FirstLowerNameChar)
import Prelude (Maybe(..))
import ElmChars (ElmChars)

data LowerName
  = LowerName FirstLowerNameChar [SubsequentNameChar]

parse :: ElmChars -> Maybe LowerName
parse elmChars =
  case parseChars elmChars of
    Nothing ->
      Nothing
    Just parsed ->
      checkForKeywords parsed 

checkForKeywords :: LowerName -> Maybe LowerName
checkForKeywords (LowerName first remainder) =
  case (first, remainder) of
    (FirstLowerNameChar.O, [SubsequentNameChar.F]) ->
