module Uppercase (Uppercase, toChar, fromChar) where


data Uppercase
    = A | B | C | D | E | F | G | H | I | J | K | L | M | N | O | P | Q | R | S | T | U | V | W | X | Y | Z

fromChar :: Char -> Maybe Uppercase
fromChar char =
    case char of
        'A' -> Just A
        'B' -> Just B
        'C' -> Just C
        'D' -> Just D
        'E' -> Just E
        'F' -> Just F
        'G' -> Just G
        'H' -> Just H
        'I' -> Just I
        'J' -> Just J
        'K' -> Just K
        'L' -> Just L
        'M' -> Just M
        'N' -> Just N
        'O' -> Just O
        'P' -> Just P
        'Q' -> Just Q
        'R' -> Just R
        'S' -> Just S
        'T' -> Just T
        'U' -> Just U
        'V' -> Just V
        'W' -> Just W
        'X' -> Just X
        'Y' -> Just Y
        'Z' -> Just Z
        _ ->
            Nothing

toChar :: Uppercase -> Char
toChar uppercase =
    case uppercase
        A -> 'A'
        B -> 'B'
        C -> 'C'
        D -> 'D'
        E -> 'E'
        F -> 'F'
        G -> 'G'
        H -> 'H'
        I -> 'I'
        J -> 'J'
        K -> 'K'
        L -> 'L'
        M -> 'M'
        N -> 'N'
        O -> 'O'
        P -> 'P'
        Q -> 'Q'
        R -> 'R'
        S -> 'S'
        T -> 'T'
        U -> 'U'
        V -> 'V'
        W -> 'W'
        X -> 'X'
        Y -> 'Y'
        Z -> 'Z'
