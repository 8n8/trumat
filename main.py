import json
import sys

# Types
ORDINARY_STRING_LITERAL_TYPE = 0
TRIPLE_QUOTED_STRING_LITERAL_TYPE = 1
FLOAT_TYPE = 2
INT_TYPE = 3


def get_elm_paths(raw):
    paths = []
    for directory_path, _, file_names in raw:
        for file_name in file_names:
            if file_name[-4:] == ".elm":
                paths.append(os.path.join(directory_path, file_name))

    return paths


def parse_ordinary_string(tokens, src, i):
    try:
        if src[i] != ord('"'):
            return i
    except IndexError:
        return i

    i += 1
    tokens['type'].append(ORDINARY_STRING_LITERAL_TYPE)
    tokens['quote_id'].append(len(tokens['type']) - 1)
    tokens['quote_start'].append(i)

    try:
        while True:
            if src[i] == ord('\\'):
                i += 2
                continue

            if src[i] == ord('"'):
                tokens['quote_end'].append(i)
                return i+1

            i += 1
    except IndexError:
        raise InvalidElm("I was parsing an ordinary string, but got the end of the input without finding the closing quote mark.")


def parse_triple_quoted_string(src, i, module_id, ast):
    # try to parse the opening """
    if parse_token(src, i, b'"""') != i + 3:
        # this isn't a triple-quoted string so stop and backtrack
        return i

    i += 3
    tokens['type'].append(TRIPLE_QUOTED_STRING_LITERAL_TYPE)
    tokens['quote_id'].append(len(tokens['type']) - 1)
    tokens['quote_start'].append(i)

    try:
        while True:
            if src[i] == ord('\\'):
                i += 2
                continue

            if parse_token(src, i, b'"""') == i + 3:
                tokens['quote_end'].append(i)
                return i+3

            i += 1

    except IndexError:
        raise InvalidElm("I was parsing a triple-quoted string, but got the end of the input without finding the closing quote marks.")


def is_digit(ch):
    return (ch >= ord('0') and ch <= ord('9')) or ch == ord('.')


def is_float_char(ch):
    return (ch >= ord('0') and ch <= ord('9')) or ch == ord('.')


def parse_float(tokens, src, i):
    try:
        if not is_float_char(src[i]):
            return i
    except IndexError:
        return i
    
    tokens['type'].append(FLOAT_TYPE)
    tokens['quote_id'].append(len(tokens['type']) - 1)
    tokens['quote_start'].append(i)

    i += 1

    try:
        while is_float_char(src[i]):
            i += 1

    except IndexError:
        pass

    tokens['quote_end'].append(i)
    return i


def is_valid_hex_char(ch):
    return (ch >= ord('a') and ch <= ord('f')) or (ch >= ord('A') and ch <= ord('F')) or is_digit(ch)


def parse_number(tokens, raw, i):
    j = parse_positive_number(tokens, raw, i)
    if j > i:
        return j

    if raw[i] != ord('-'):
        return i

    i += 1
    return parse_positive_number(tokens, raw, i)


def parse_positive_number(tokens, raw, i):
    if not is_digit(raw[i]):
        return i

    tokens['quote_start'].append(i) 
    i += 1

    is_float = False
    while i < len(raw):
        if is_digit(raw[i]) or is_hex_letter(raw[i]) or raw[i] == ord('x'):
            i += 1
            continue

        if raw[i] == ord('.'):
            is_float = True
            i += 1
            continue

        if raw[i] == ord('e'):
            is_float = True
            i += 1
            continue

        break

    tokens['quote_end'].append(i)
    if is_float:
        tokens['type'].append(FLOAT_TYPE)
        return i

    tokens['type'].append(INT_TYPE)
    tokens['quote_id'].append(len(tokens['type']) - 1)
    return i


def is_subsequent_name_char(ch):
    return (
        (ch >= ord('a') and ch <= ord('z'))
        or (ch >= ord('A') and ch <= ord('Z'))
        or ch == ord('_'))


def parse_capitalised_name(tokens, raw, i):
    try:
        if not (src[i] >= ord('A') and src[i] <= ord('Z')):
            return i
    except IndexError:
        return i
    tokens['quote_start'].append(i)
    i += 1

    while i < len(raw):
        if not is_subsequent_name_char(raw[i]):
            break
        i += 1

    tokens['quote_end'].append(i)
    tokens['type'].append(CAPITALISED_NAME_TYPE)
    tokens['quote_id'].append(len(token['type']) - 1)
    return i


def parse_variable_name(tokens, raw, i):
    try:
        if not (
            (src[i] >= ord('a') and src[i] <= ord('z')
            or src[i] == ord('_')):
            return i
    except IndexError:
        return i
    tokens['quote_start'].append(i)
    i += 1

    while i < len(raw):
        if not is_subsequent_name_char(raw[i]):
            break
        i += 1

    tokens['quote_end'].append(i)
    tokens['type'].append(VARIABLE_NAME_TYPE)
    tokens['quote_id'].append(len(tokens['type'])-1)
    return i


keywords = [
    ("module", MODULE),
    ("import", IMPORT),
    ("exposing", EXPOSING),
    ("=", EQUALS),
    (":", COLON),
    ("->", ARROW),
    ("if", IF),
    ("else", ELSE),
    ("as", AS),
    ("case", CASE),
    ("of", OF),
    (",", COMMA),
    ("[", OPEN_SQUARE_BRACKET),
    ("]", CLOSE_SQUARE_BRACKET),
    ("(", OPEN_PARENTHESIS),
    (")", CLOSE_PARENTHESIS),
    ("<=", LESS_OR_EQUAL),
    (">=", MORE_OR_EQUAL),
    ("<", LESS_THAN),
    (">", MORE_THAN),
    ("/=", NOT_EQUAL),
    ("==", EQUAL),
    ("%", MODULUS),
    ("&&", AND),
    ("||", OR),
    ("not", NOT),
    ("*", MULTIPLY),
    ("+", PLUS),
    ("-", MINUS),
    ("//", DIVIDE_INT),
    ("/", DIVIDE_FLOAT)
]


def parse_keyword(tokens, raw, i):
    for keyword, type_ in keywords:
        j = parse_exact(raw, i, keyword)
        if j > i:
            tokens['type'].append(type_)
            return j

        i = j

    return i


def parse_line_comment(tokens, raw, i):
    j = parse_exact(raw, i, "--")
    if j == i:
        return i
    i = j

    tokens['type'].append(LINE_COMMENT)
    tokens['quote_id'].append(len(tokens['type']) - 1)
    tokens['quote_start'].append(i)

    while i < len(raw):
        if src[i] == ord('\n'):
            break

        i += 1

    tokens['quote_end'].append(i)
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
    
    tokens['type'].append(BLOCK_COMMENT)
    tokens['quote_id'].append(len(tokens['type']) - 1)
    tokens['quote_start'].append(i)

    while True:
        if parse_exact(raw, i, "-}") == i:
            i += 1
            continue

        break 

    tokens['quote_end'].append(i)
    return i+2


token_parsers = [
    parse_meaningful_whitespace,
    parse_meaningless_whitespace,
    parse_line_comment,
    parse_doc_comment,
    parse_block_comment,
    parse_keyword,
    parse_capitalised_name,
    parse_variable_name,
    parse_number,
    parse_triple_quoted_string,
    parse_ordinary_string
]


def parse_token(tokens, raw, i):
    for parser in token_parsers:
        j = parser(tokens, raw, i)
        if j > i:
            return j

        i = j

    return i


def tokenize(raw):
    tokens = {
        'type': array.array('B'),
        'quote_id': array.array('L'),
        'quote_start': array.array('L'),
        'quote_end': array.array('L')}

    i = 0
    while i < len(raw):
        i = parse_token(tokens, raw, i)

    return tokens


def read_sources(source_directories):
    src = b''
    for source_directory in elm_dot_json['source-directories']:
        for path in get_elm_paths(os.walk(source_directory)):
            with open(path, 'rb') as elm:
                src += bytes([255]) + f.read()

    return src


def main():
    with open('elm.json', 'rb') as f:
        elm_dot_json = json.load(f)

    src = read_sources(elm_dot-json['source-directories'])

    ast = parse(tokenize(src))

main()


def parse_exact(src, i, token):
    j = 0

    try:
        while j < len(token):
            if src[i+j] != token[j]:
                # a source character doesn't match a token character
                return i

            j += 1
    except IndexError:
        # source is too short
        return i

    # happy path: all the tokens matched and the source was long enough
    return i + j
