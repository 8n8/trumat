module Name (Name) where

import Alphabet (Alphabet)

data Name
    = Name First


data First
    = Uppercase Alphabet
    | Lowercase Alphabet
