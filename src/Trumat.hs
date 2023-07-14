module Trumat (format) where

import Data.Text (Text)
import qualified Data.Text as Text

data State
    = Init Text
    | AfterModuleKeyword
    | InModuleName
    | AfterModuleName
    | ModuleExposing Text
    | AfterExposingKeyword
    deriving Show

step :: Char -> State -> Action
step '(' AfterExposingKeyword =
  
step ' ' (ModuleExposing "exposing") =
  Commit AfterExposingKeyword "exposing "
step 'g' (ModuleExposing "exposin") =
  DoNothing (ModuleExposing "exposing")
step 'n' (ModuleExposing "exposi") =
  DoNothing (ModuleExposing "exposin")
step 'i' (ModuleExposing "expos") =
  DoNothing (ModuleExposing "exposi")
step 's' (ModuleExposing "expo") =
  DoNothing (ModuleExposing "expos")
step 'o' (ModuleExposing "exp") =
  DoNothing (ModuleExposing "expo")
step 'p' (ModuleExposing "ex") =
  DoNothing (ModuleExposing "exp")
step 'x' (ModuleExposing "e") =
  DoNothing (ModuleExposing "ex")
step 'e' AfterModuleName =
  DoNothing (ModuleExposing "e")
step 'm' (Init "") =
  DoNothing (Init "m") 
step 'o' (Init "m") =
  DoNothing (Init "mo")
step 'd' (Init "mo") =
  DoNothing (Init "mod")
step 'u' (Init "mod") =
  DoNothing (Init "modu")
step 'l' (Init "modu") =
  DoNothing (Init "modul")
step 'e' (Init "modul") =
  DoNothing (Init "module")
step ' ' (Init "module") =
  Commit AfterModuleKeyword "module "
step 'X' AfterModuleKeyword =
  Commit InModuleName "X"
step ' ' InModuleName =
  Commit AfterModuleName " "
step ch state =
  Fail ("bad char: " <> [ch] <> " and bad state: " <> show state)

format :: Text -> Either String Text
format unformatted =
  formatHelp unformatted (Init "") ""

formatHelp :: Text -> State -> Text -> Either String Text
formatHelp unformatted state formatted =
  case Text.uncons unformatted of
    Nothing ->
      Right formatted

    Just (head', tail') ->
      case step head' state of
        DoNothing newState ->
          formatHelp tail' newState formatted

        Commit newState fragment ->
          formatHelp tail' newState (formatted <> fragment)

        Fail error' ->
          Left error'


data Action
    = DoNothing State
    | Commit State Text
    | Fail String
