module Space (parse) where

import Prelude
import Data.Void (Void)
import Text.Megaparsec (Parsec, takeP, token, lookAhead)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified ElmChar
import ElmChar (ElmChar)
import Text.Megaparsec.Debug (dbg)

type Parser =
    Parsec Void Text

parse :: Parser ()
parse =
  dbg "parse" $
    parseHelp Running


parseHelp :: State -> Parser ()
parseHelp state =
    do
    ch <- lookAhead ElmChar.parse
    let newState = machine state ch
    case newState of
        Running ->
            do
            _ <- ElmChar.parse
            parseHelp newState

        Finished ->
            return ()


data State
    = Running
    | Finished


machine :: State -> ElmChar -> State
machine state char =
    case state of
        Finished ->
            Finished

        Running ->
            runningMachine char


runningMachine :: ElmChar -> State
runningMachine char =
    case char of
        ElmChar.Au -> Finished
        ElmChar.Bu -> Finished
        ElmChar.Cu -> Finished
        ElmChar.Du -> Finished
        ElmChar.Eu -> Finished
        ElmChar.Fu -> Finished
        ElmChar.Gu -> Finished
        ElmChar.Hu -> Finished
        ElmChar.Iu -> Finished
        ElmChar.Ju -> Finished
        ElmChar.Ku -> Finished
        ElmChar.Lu -> Finished
        ElmChar.Mu -> Finished
        ElmChar.Nu -> Finished
        ElmChar.Ou -> Finished
        ElmChar.Pu -> Finished
        ElmChar.Qu -> Finished
        ElmChar.Ru -> Finished
        ElmChar.Su -> Finished
        ElmChar.Tu -> Finished
        ElmChar.Uu -> Finished
        ElmChar.Vu -> Finished
        ElmChar.Wu -> Finished
        ElmChar.Xu -> Finished
        ElmChar.Yu -> Finished
        ElmChar.Zu -> Finished
        ElmChar.Al -> Finished
        ElmChar.Bl -> Finished
        ElmChar.Cl -> Finished
        ElmChar.Dl -> Finished
        ElmChar.El -> Finished
        ElmChar.Fl -> Finished
        ElmChar.Gl -> Finished
        ElmChar.Hl -> Finished
        ElmChar.Il -> Finished
        ElmChar.Jl -> Finished
        ElmChar.Kl -> Finished
        ElmChar.Ll -> Finished
        ElmChar.Ml -> Finished
        ElmChar.Nl -> Finished
        ElmChar.Ol -> Finished
        ElmChar.Pl -> Finished
        ElmChar.Ql -> Finished
        ElmChar.Rl -> Finished
        ElmChar.Sl -> Finished
        ElmChar.Tl -> Finished
        ElmChar.Ul -> Finished
        ElmChar.Vl -> Finished
        ElmChar.Wl -> Finished
        ElmChar.Xl -> Finished
        ElmChar.Yl -> Finished
        ElmChar.Zl -> Finished
        ElmChar.Space -> Running
        ElmChar.Newline -> Running
        ElmChar.DoubleQuote -> Finished
        ElmChar.SingleQuote -> Finished
        ElmChar.Pipe -> Finished
        ElmChar.OpenCurly -> Finished
        ElmChar.CloseCurly -> Finished
        ElmChar.At -> Finished
        ElmChar.Equals -> Finished
        ElmChar.Hyphen -> Finished
        ElmChar.CloseParens -> Finished
        ElmChar.Comma -> Finished
        ElmChar.OpenParens -> Finished
        ElmChar.D0 -> Finished
        ElmChar.D1 -> Finished
        ElmChar.D2 -> Finished
        ElmChar.D3 -> Finished
        ElmChar.D4 -> Finished
        ElmChar.D5 -> Finished
        ElmChar.D6 -> Finished
        ElmChar.D7 -> Finished
        ElmChar.D8 -> Finished
        ElmChar.D9 -> Finished
