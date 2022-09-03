import json
import os
import array

MODULE_SEPARATOR = 255


class TOKEN:
    ORDINARY_STRING_LITERAL = 0
    TRIPLE_QUOTED_STRING_LITERAL = 1
    FLOAT = 2
    INT = 3
    MODULE = 4
    EXPOSING = 5
    EQUALS = 6
    COLON = 7
    ARROW = 8
    IF = 9
    ELSE = 10
    AS = 11
    CASE = 12
    OF = 13
    COMMA = 14
    OPEN_SQUARE_BRACKET = 15
    CLOSE_SQUARE_BRACKET = 16
    OPEN_PARENTHESIS = 17
    CLOSE_PARENTHESIS = 18
    LESS_OR_EQUAL = 19
    MORE_OR_EQUAL = 20
    LESS_THAN = 21
    MORE_THAN = 22
    NOT_EQUAL = 23
    EQUAL = 24
    MODULUS = 25
    AND = 26
    OR = 27
    NOT = 28
    MULTIPLY = 29
    PLUS = 30
    MINUS = 31
    DIVIDE_INT = 32
    DIVIDE_FLOAT = 33
    IMPORT = 34
    MODULE_SEPARATOR = 35


def get_elm_paths(raw):
    paths = []
    for directory_path, _, file_names in raw:
        for file_name in file_names:
            if file_name[-4:] == ".elm":
                paths.append(os.path.join(directory_path, file_name))

    return paths


def parse_module_separator(tokens, src, i):
    try:
        if src[i] != MODULE_SEPARATOR:
            return i
    except IndexError:
        return i

    tokens["type"].append(TOKEN.MODULE_SEPARATOR)
    tokens["start"].append(i)
    i += 1
    tokens["end"].append(i)
    return i


def parse_ordinary_string(tokens, src, i):
    try:
        if src[i] != ord('"'):
            return i
    except IndexError:
        return i

    i += 1
    tokens["type"].append(TOKEN.ORDINARY_STRING_LITERAL)
    tokens["start"].append(i)

    while True:
        if src[i] == ord("\\"):
            i += 2
            continue

        if src[i] == ord('"'):
            tokens["end"].append(i)
            return i + 1

        i += 1


def parse_triple_quoted_string(tokens, src, i):
    # try to parse the opening """
    if parse_exact(src, i, b'"""') != i + 3:
        # this isn't a triple-quoted string so stop and backtrack
        return i

    i += 3
    tokens["type"].append(TOKEN.TRIPLE_QUOTED_STRING_LITERAL)
    tokens["start"].append(i)

    while True:
        if src[i] == ord("\\"):
            i += 2
            continue

        if parse_exact(src, i, b'"""') == i + 3:
            tokens["end"].append(i)
            return i + 3

        i += 1


def is_digit(ch):
    return (ch >= ord("0") and ch <= ord("9")) or ch == ord(".")


def is_float_char(ch):
    return (ch >= ord("0") and ch <= ord("9")) or ch == ord(".")


def is_hex_char(ch):
    return (
        (ch >= ord("a") and ch <= ord("f"))
        or (ch >= ord("A") and ch <= ord("F"))
        or is_digit(ch)
    )


def parse_number(tokens, raw, i):
    j = parse_positive_number(tokens, raw, i)
    if j > i:
        return j

    if raw[i] != ord("-"):
        return i

    i += 1
    return parse_positive_number(tokens, raw, i)


def parse_positive_number(tokens, raw, i):
    if not is_digit(raw[i]):
        return i

    tokens["start"].append(i)
    i += 1

    is_float = False
    while i < len(raw):
        if is_digit(raw[i]) or is_hex_char(raw[i]) or raw[i] == ord("x"):
            i += 1
            continue

        if raw[i] == ord("."):
            is_float = True
            i += 1
            continue

        if raw[i] == ord("e"):
            is_float = True
            i += 1
            continue

        break

    tokens["end"].append(i)
    if is_float:
        tokens["type"].append(TOKEN.FLOAT)
        return i

    tokens["type"].append(TOKEN.INT)
    return i


def is_subsequent_name_char(ch):
    return (
        (ch >= ord("a") and ch <= ord("z"))
        or (ch >= ord("A") and ch <= ord("Z"))
        or ch == ord("_")
    )


def parse_capitalised_name(tokens, src, i):
    try:
        if not (src[i] >= ord("A") and src[i] <= ord("Z")):
            return i
    except IndexError:
        return i
    tokens["start"].append(i)
    i += 1

    while i < len(src):
        if not is_subsequent_name_char(src[i]):
            break
        i += 1

    tokens["end"].append(i)
    tokens["type"].append(TOKEN.CAPITALISED_NAME)
    return i


def parse_variable_name(tokens, src, i):
    try:
        if not ((src[i] >= ord("a") and src[i] <= ord("z")) or src[i] == ord("_")):
            return i
    except IndexError:
        return i
    tokens["start"].append(i)
    i += 1

    while i < len(src):
        if not is_subsequent_name_char(src[i]):
            break
        i += 1

    tokens["end"].append(i)
    tokens["type"].append(TOKEN.VARIABLE_NAME)
    return i


keywords = [
    ("module", TOKEN.MODULE),
    ("import", TOKEN.IMPORT),
    ("exposing", TOKEN.EXPOSING),
    ("=", TOKEN.EQUALS),
    (":", TOKEN.COLON),
    ("->", TOKEN.ARROW),
    ("if", TOKEN.IF),
    ("else", TOKEN.ELSE),
    ("as", TOKEN.AS),
    ("case", TOKEN.CASE),
    ("of", TOKEN.OF),
    (",", TOKEN.COMMA),
    ("[", TOKEN.OPEN_SQUARE_BRACKET),
    ("]", TOKEN.CLOSE_SQUARE_BRACKET),
    ("(", TOKEN.OPEN_PARENTHESIS),
    (")", TOKEN.CLOSE_PARENTHESIS),
    ("<=", TOKEN.LESS_OR_EQUAL),
    (">=", TOKEN.MORE_OR_EQUAL),
    ("<", TOKEN.LESS_THAN),
    (">", TOKEN.MORE_THAN),
    ("/=", TOKEN.NOT_EQUAL),
    ("==", TOKEN.EQUAL),
    ("%", TOKEN.MODULUS),
    ("&&", TOKEN.AND),
    ("||", TOKEN.OR),
    ("not", TOKEN.NOT),
    ("*", TOKEN.MULTIPLY),
    ("+", TOKEN.PLUS),
    ("-", TOKEN.MINUS),
    ("//", TOKEN.DIVIDE_INT),
    ("/", TOKEN.DIVIDE_FLOAT),
]


def parse_keyword(tokens, raw, i):
    for keyword, type_ in keywords:
        j = parse_exact(raw, i, keyword)
        if j > i:
            tokens["start"].append(i)
            tokens["type"].append(type_)
            tokens["end"].append(j)
            return j

        i = j

    return i


def parse_line_comment(tokens, src, i):
    j = parse_exact(src, i, "--")
    if j == i:
        return i
    i = j

    tokens["type"].append(TOKEN.LINE_COMMENT)
    tokens["start"].append(i)

    while i < len(src):
        if src[i] == ord("\n"):
            break

        i += 1

    tokens["end"].append(i)
    return i


def parse_block_comment(tokens, raw, i):
    return parse_block_comment_help(tokens, raw, i, "{-")


def parse_doc_comment(tokens, raw, i):
    return parse_block_comment_help(tokens, raw, i, "{-|")


def parse_block_comment_help(tokens, raw, i, start):
    j = parse_exact(raw, i, start)
    if j == i:
        return i
    i = j

    tokens["type"].append(TOKEN.BLOCK_COMMENT)
    tokens["start"].append(i)

    while True:
        if parse_exact(raw, i, "-}") == i:
            i += 1
            continue

        break

    tokens["end"].append(i)
    return i + 2


def parse_spaces(tokens, src, i):
    return parse_spaces_help(tokens, src, i, " ", TOKEN.SPACES)


def parse_newlines(tokens, src, i):
    return parse_spaces_help(tokens, src, i, "\n", TOKEN.NEWLINES)


def parse_spaces_help(tokens, raw, i, char, type_):
    try:
        if raw[i] != char:
            return i
    except IndexError:
        return i

    tokens["start"].append(i)
    i += 1

    tokens["type"].append(type_)
    while True:
        try:
            if raw[i] != char:
                break
        except IndexError:
            break

        i += 1

    tokens["end"].append(i)
    return i


def parse_many(tokens, raw, i, parsers):
    for parser in parsers:
        j = parser(tokens, raw, i)
        if j > i:
            return j

        i = j

    return i


def tokenize(raw):
    tokens = {
        "type": array.array("B"),
        "start": array.array("L"),
        "end": array.array("L"),
    }

    i = 0
    while i < len(raw):
        i = parse_many(
            tokens,
            raw,
            i,
            [
                parse_spaces,
                parse_newlines,
                parse_line_comment,
                parse_block_comment,
                parse_keyword,
                parse_capitalised_name,
                parse_variable_name,
                parse_number,
                parse_triple_quoted_string,
                parse_ordinary_string,
                parse_module_separator,
            ],
        )

    return tokens


def read_sources(source_directories):
    src = b""
    for source_directory in source_directories:
        for path in get_elm_paths(os.walk(source_directory)):
            with open(path, "rb") as elm:
                src += bytes([MODULE_SEPARATOR]) + elm.read()

    return src


def main():
    with open("elm.json", "rb") as f:
        elm_dot_json = json.load(f)

    src = read_sources(elm_dot_json["source-directories"])

    ast = parse(tokenize(src))


main()


def parse_exact(src, i, token):
    j = 0

    try:
        while j < len(token):
            if src[i + j] != token[j]:
                # a source character doesn't match a token character
                return i

            j += 1
    except IndexError:
        # source is too short
        return i

    # happy path: all the tokens matched and the source was long enough
    return i + j
