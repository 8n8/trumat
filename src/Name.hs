module Name (toString, First(..), Subsequent(..)) where

import Lowercase (Lowercase)
import Data.Text (Text)
import qualified Data.Text as Text
import Digit (Digit)
import qualified Digit
import qualified Uppercase
import qualified Lowercase
import Uppercase (Uppercase)


data First
    = UnderscoreF
    | LowercaseF Lowercase
    | UppercaseF Uppercase


firstToChar :: First -> Char
firstToChar first =
    case first of
        UnderscoreF ->
            '_'

        LowercaseF lowercase ->
            Lowercase.toChar lowercase

        UppercaseF uppercase ->
            Uppercase.toChar uppercase


data Subsequent
    = LowercaseS Lowercase
    | UppercaseS Uppercase
    | Digit Digit
    | UnderscoreS


subsequentToChar :: Subsequent -> Char
subsequentToChar subsequent =
    case subsequent of
        LowercaseS lowercase ->
            Lowercase.toChar lowercase

        UppercaseS uppercase ->
            Uppercase.toChar uppercase

        Digit digit ->
            Digit.toChar digit

        UnderscoreS ->
            '_'


toString :: First -> [Subsequent] -> Text
toString first subsequent =
    Text.pack (firstToChar first : map subsequentToChar subsequent)
