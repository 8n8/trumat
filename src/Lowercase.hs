module Lowercase (Lowercase (..), fromChar, toChar) where

data Lowercase = A | B | C | D | E | F | G | H | I | J | K | L | M | N | O | P | Q | R | S | T | U | V | W | X | Y | Z

fromChar :: Char -> Maybe Lowercase
fromChar char =
  case char of
    'a' -> Just A
    'b' -> Just B
    'c' -> Just C
    'd' -> Just D
    'e' -> Just E
    'f' -> Just F
    'g' -> Just G
    'h' -> Just H
    'i' -> Just I
    'j' -> Just J
    'k' -> Just K
    'l' -> Just L
    'm' -> Just M
    'n' -> Just N
    'o' -> Just O
    'p' -> Just P
    'q' -> Just Q
    'r' -> Just R
    's' -> Just S
    't' -> Just T
    'u' -> Just U
    'v' -> Just V
    'w' -> Just W
    'x' -> Just X
    'y' -> Just Y
    'z' -> Just Z
    _ -> Nothing

toChar :: Lowercase -> Char
toChar lowercase =
  case lowercase of
    A -> 'a'
    B -> 'b'
    C -> 'c'
    D -> 'd'
    E -> 'e'
    F -> 'f'
    G -> 'g'
    H -> 'h'
    I -> 'i'
    J -> 'j'
    K -> 'k'
    L -> 'l'
    M -> 'm'
    N -> 'n'
    O -> 'o'
    P -> 'p'
    Q -> 'q'
    R -> 'r'
    S -> 's'
    T -> 't'
    U -> 'u'
    V -> 'v'
    W -> 'w'
    X -> 'x'
    Y -> 'y'
    Z -> 'z'
