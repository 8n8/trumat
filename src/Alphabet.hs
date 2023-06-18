module Alphabet (Alphabet(..), toUppercase, toLowercase) where


data Alphabet
    = A | B | C | D | E | F | G | H | I | J | K | L | M | N | O | P | Q | R | S | T | U | V | W | X | Y | Z

fromUppercase :: Char -> Maybe Alphabet
fromUppercase char =
    case char of
        'A' ->
            Just A
        'B' ->
            Just B
        'C' ->
            Just C

toLowercase :: Alphabet -> Char
toLowercase alphabet =
    case alphabet of
        A ->
            'a'
        B ->
            'b'
        C ->
            'c'
        D ->
            'd'
        E ->
            'e'
        F ->
            'f'
        G ->
            'g'
        H ->
            'h'
        I ->
            'i'
        J ->
            'j'
        K ->
            'k'
        L ->
            'l'
        M ->
            'm'
        N ->
            'n'
        O ->
            'o'
        P ->
            'p'
        Q ->
            'q'
        R ->
            'r'
        S ->
            's'
        T ->
            't'
        U ->
            'u'
        V ->
            'v'
        W ->
            'w'
        X ->
            'x'
        Y ->
            'y'
        Z ->
            'z'

toUppercase :: Alphabet -> Char
toUppercase alphabet =
    case alphabet of
        A ->
            'A'

        B ->
            'B'

        C ->
            'C'
        D ->
            'D'
        E ->
            'E'
        F ->
            'F'
        G ->
            'G'
        H ->
            'H'
        I ->
            'I'
        J ->
            'J'
        K ->
            'K'
        L ->
            'L'
        M ->
            'M'
        N ->
            'N'
        O ->
            'O'
        P ->
            'P'
        Q ->
            'Q'
        R ->
            'R'
        S ->
            'S'
        T ->
            'T'
        U ->
            'U'
        V ->
            'V'
        W ->
            'W'
        X ->
            'X'
        Y ->
            'Y'
        Z ->
            'Z'
