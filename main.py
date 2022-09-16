import json
import os
import array

MODULE_SEPARATOR = 255


ORDINARY_STRING_LITERAL_TOKEN = 0
TRIPLE_QUOTED_STRING_LITERAL_TOKEN = 1
FLOAT_TOKEN = 2
INT_TOKEN = 3
MODULE_TOKEN = 4
EXPOSING_TOKEN = 5
EQUALS_TOKEN = 6
COLON_TOKEN = 7
ARROW_TOKEN = 8
IF_TOKEN = 9
ELSE_TOKEN = 10
AS_TOKEN = 11
CASE_TOKEN = 12
OF_TOKEN = 13
COMMA_TOKEN = 14
OPEN_SQUARE_BRACKET_TOKEN = 15
CLOSE_SQUARE_BRACKET_TOKEN = 16
OPEN_PARENTHESIS_TOKEN = 17
CLOSE_PARENTHESIS_TOKEN = 18
LESS_OR_EQUAL_TOKEN = 19
MORE_OR_EQUAL_TOKEN = 20
LESS_THAN_TOKEN = 21
MORE_THAN_TOKEN = 22
NOT_EQUAL_TOKEN = 23
EQUAL_TOKEN = 24
MODULUS_TOKEN = 25
AND_TOKEN = 26
OR_TOKEN = 27
NOT_TOKEN = 28
MULTIPLY_TOKEN = 29
PLUS_TOKEN = 30
MINUS_TOKEN = 31
DIVIDE_INT_TOKEN = 32
DIVIDE_FLOAT_TOKEN = 33
IMPORT_TOKEN = 34
MODULE_SEPARATOR_TOKEN = 35
NEWLINES_TOKEN = 36
BLOCK_COMMENT_TOKEN = 37
LINE_COMMENT_TOKEN = 38
VARIABLE_NAME_TOKEN = 39
CAPITALISED_NAME_TOKEN = 40
EXPOSE_ALL_TOKEN = 41
DOT_TOKEN = 42


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

    tokens["type"].append(MODULE_SEPARATOR_TOKEN)
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
    tokens["type"].append(ORDINARY_STRING_LITERAL_TOKEN)
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
    tokens["type"].append(TRIPLE_QUOTED_STRING_LITERAL_TOKEN)
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
        tokens["type"].append(FLOAT_TOKEN)
        return i

    tokens["type"].append(INT_TOKEN)
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
    tokens["type"].append(CAPITALISED_NAME_TOKEN)
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
    tokens["type"].append(VARIABLE_NAME_TOKEN)
    return i


keywords = [
    ("module", MODULE_TOKEN),
    ("import", IMPORT_TOKEN),
    ("exposing", EXPOSING_TOKEN),
    ("=", EQUALS_TOKEN),
    (":", COLON_TOKEN),
    ("->", ARROW_TOKEN),
    ("if", IF_TOKEN),
    ("else", ELSE_TOKEN),
    ("as", AS_TOKEN),
    ("case", CASE_TOKEN),
    ("of", OF_TOKEN),
    (",", COMMA_TOKEN),
    ("[", OPEN_SQUARE_BRACKET_TOKEN),
    ("]", CLOSE_SQUARE_BRACKET_TOKEN),
    ("(", OPEN_PARENTHESIS_TOKEN),
    (")", CLOSE_PARENTHESIS_TOKEN),
    ("<=", LESS_OR_EQUAL_TOKEN),
    (">=", MORE_OR_EQUAL_TOKEN),
    ("<", LESS_THAN_TOKEN),
    (">", MORE_THAN_TOKEN),
    ("/=", NOT_EQUAL_TOKEN),
    ("==", EQUAL_TOKEN),
    ("%", MODULUS_TOKEN),
    ("&&", AND_TOKEN),
    ("||", OR_TOKEN),
    ("not", NOT_TOKEN),
    ("*", MULTIPLY_TOKEN),
    ("+", PLUS_TOKEN),
    ("-", MINUS_TOKEN),
    ("//", DIVIDE_INT_TOKEN),
    ("/", DIVIDE_FLOAT_TOKEN),
    ("..", EXPOSE_ALL_TOKEN),
    (".", DOT_TOKEN),
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

    tokens["type"].append(LINE_COMMENT_TOKEN)
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

    tokens["type"].append(BLOCK_COMMENT_TOKEN)
    tokens["start"].append(i)

    while True:
        if parse_exact(raw, i, "-}") == i:
            i += 1
            continue

        break

    tokens["end"].append(i)
    return i + 2


def parse_spaces(_, src, i):
    while True:
        if src[i] != ord(" "):
            return i

        i += 1


def parse_newlines(tokens, src, i):
    if raw[i] != ord("\n"):
        return i

    tokens["start"].append(i)
    i += 1

    tokens["type"].append(type_)
    while True:
        if raw[i] != ord("\n"):
            break

        i += 1

    tokens["end"].append(i)
    return i


def choice(tokens, raw, i, parsers):
    for parser in parsers:
        j = parser(tokens, raw, i)
        if j > i:
            return j

        i = j

    return i


def tokenize(src):
    tokens = {
        "type": array.array("B"),
        "start": array.array("L"),
        "end": array.array("L"),
    }

    i = 0
    while i < len(src):
        i = choice(
            tokens,
            src,
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



def init_ast():
    return {
        # the order things appear in the source file
        # this will contain 4 bytes for every AST item
        # say that there are 3 AST items per line of code,
        # and there are around 10^6 lines of code
        #
        # that comes to 4 * 3 * 10 **6 = 12_000_000
        "ordering": array.array("L", b"\x00" * 12_000_000),
        "num_orderings": 0,

        # name
        # say there are around 10^6 lines of code, and a new
        # name every couple of lines
        #
        # that comes to 4 * 10^6 / 2 = 2_000_000 B
        "named_id": array.array("L", b"\x00" * 2_000_000),
        "name_start": array.array("L", b"\x00" * 2_000_000),
        "name_end": array.array("L", b"\x00" * 2_000_000),
        "num_names": 0,

        # block comment
        # say there is a block comment every 10 lines of code
        # on average, and 10^6 lines of code, that comes to
        #
        # 4 * 10^6 / 10 = 400_000
        "block_comment_start": array.array("L", b"\x00" * 400_000),
        "block_comment_end": array.array("L", b"\x00" * 400_000),
        "num_block_comments": 0,

        # line comment
        # say there are twice as many line comments as block
        # comments, that is 200_000
        "line_comment_start": array.array("L", b"\x00" * 200_000),
        "line_comment_end": array.array("L", b"\x00" * 200_000),
        "num_line_comments": 0,

        # argument
        # say there is an argument given twice on each line
        # of code, for 10^6 lines of code, that is
        #
        # 4 * 2 * 10^6 = 8_000_000
        "argumented": array.array("L", b"\x00" * 8_000_000),
        "argument": array.array("L", b"\x00" * 8_000_000),
        "num_arguments": 0,

        # parameter
        # say there is a parameter every 3 lines of code, for
        # 10^6 lines of code, that is
        #
        # 4 * 10^6 / 3 ~= 2_000_000
        "parametered": array.array("L", b"\x00" * 2_000_000),
        "num_parameters": 0,

        # module member
        # say there is a top-level declaration every 10 lines
        # of code on average, for 10^6 lines of code, that is
        # 2 * 10^6 / 10 = 200_000
        "module_id": array.array("H", b"\x00" * 200_000),
        # 4 * 10^6 / 10 = 400_000
        "member": array.array("L", b"\x00" * 400_000),
        "num_module_members": 0,

        # custom type branch
        # say there is a new custom type every 30 lines of
        # code on average, with 5 branches, for 10^6 lines of
        # code, that comes to
        #
        # 4 * 10^6 / 30 ~= 200_000
        "parent_type_id": array.array("L", b"\x00" * 200_000),
        "num_custom_type_branch": 0,
        
        # type signature member
        # say there is a type signature every 5 lines of
        # code on average for 10^6 lines of code, and that
        # each type signature averages 4 items, that comes to
        #
        # 4 * 4 * 10^6 / 5 = 3_200_000
        "type_signature_member": array.array("L", b"\x00" * 3_200_000),
        "num_type_signature_members": 0,

        # switch branch
        # say there is a switch (case-of or if-else) every
        # 20 lines of code in 10^6 lines of code, and that
        # the average switch has 4 branches
        #
        # that comes to 4 * 4 * 10^6 / 20 = 800_000
        "switched_on_value": array.array("L", b"\x00" * 800_000),
        "pattern_value": array.array("L", b"\x00" * 800_000),
        "result": array.array("L", b"\x00" * 800_000),
        "num_switch_branches": 0,

        # switch IDs that are if - else
        #
        # say that 1/2 of the switch branches are if-else
        # that comes to 400_000
        "if_else_id": array.array("L", b"\x00" * 400_000),
        "num_if_else": 0,

        # module export
        # say that the average module contains 500 lines of
        # code in 10^6 lines of code, and that the average
        # module exports 5 items, that comes to
        #
        # 4 * 5 * 10^6 / 500 = 40_000
        "exported": array.array("L", b"\x00" * 40_000),
        "num_exported": 0,

        # export whole type
        # say that each module exports 2 whole types, that 
        # comes to
        #
        # 4 * 2 * 10^6 / 500 = 16_000
        "export_whole_type": array.array("L", b"\x00" * 16_000),
        "num_export_whole_type": 0,

        # export all of a module
        # say that one in five modules exports everything
        # that comes to
        #
        # 4 * 10^6 / (500 * 5) = 1600
        "export_all_module": array.array("L", b"\x00" * 1600),
        "num_export_all_module": 0,

        # module import single
        # say the average module has 10 import rows, and each
        # row pulls in 10 names, that comes to
        #
        # 4 * 10 * 10 * 10^6 / 500 = 800_000
        "import_single_into": array.array("L", b"\x00" * 800_000),
        "imported_item": array.array("L", b"\x00" * 800_000),
        "num_import_single": 0,

        # import all of a module
        # say there is 1 import all statement in each module
        #
        # that comes to 4 * 10^6 / 500 = 8000
        "import_all_into": array.array("L", b"\x00" * 8000),
        "imported_module": array.array("L", b"\x00" * 8000),
        "num_import_all": 0,

        # return value of a let in expression
        # say there is a let-in statement every 20 lines,
        # that comes to
        #
        # 4 * 10^6 / 20 = 200_000
        "let-in_id": array.array("L", b"\x00" * 200_000),
        "in_id": array.array("L", b"\x00" * 200_000),
        "num_let_in": 0,

        # list literal
        # say there is a list literal every 20 lines of code
        # that comes to
        #
        # 4 * 10^6 / 20 = 200_000
        "id_of_first_list_item": array.array("L", b"\x00" * 200_000),
        "list_length": array.array("L", b"\x00" * 200_000),
        "num_lists": 0,

        # wrapper
        # say there is a wrapper every 3 lines of code, that
        # comes to
        # 4 * 10^6 / 3 = 1_400_000
        "wrapped": array.array("L", b"\x00" * 1_400_000)
        "num_wrappers": 0,

        # record type declaration member
        # say there is a record declaration every 20 lines of
        # code, and each record has 10 items, that comes to
        # 4 * 10 * 10^6 / 20 = 2_000_000
        "record_id": array.array("L", b"\x00" * 2_000_000),
        "num_record_members": 0,
    }


def ast_choice(ast, tokens, i, parsers):
    for parser in parsers:
        j = parser(ast, tokens, i)
        if j > i:
            return j

        i = j

    return i


def parse_value_ref(value_ref):
    """
    A value ref is 4 bytes. The least significant byte is the
    type and the most significant three bytes are the ID.
    """
    id_ = value_ref >> 8
    type_ = value_ref & 0xFF
    return id_, type_


def get_max_id(ast):
    max_id = 0
    found = False

    for i in range(ast["num_names"]):
        old_value_ref = ast["named_id"][i]
        old_id, old_type = parse_value_ref(old_value_ref)
        if old_type == MODULE_TYPE:
            if old_id > max_id:
                max_id = old_id
                found = True

    return max_id, found


def get_new_module_id(ast):
    max_id, found = get_max_id(ast)
    if found:
        return max_id + 1

    return 0


def eat_up_any_whitespace(tokens, i):
    while True:
        if tokens[i] == NEWLINES_TOKEN:
            i += 1
            continue

        break

    return i


def get_new_parent_type_id(ast):
    new_id = 0
    for id in ast["parent_type_id"]:
        if id > new_id:
            new_id = id

    return new_id


def insert_name(x):
    x["ast"]["named_id"][x["ast"]["num_names"]] = x["id"]
    x["ast"]["name_start"][x["ast"]["num_names"]] = x["tokens"][x["i"]]["start"]
    x["ast"]["name_end"][x["ast"]["num_names"]] = x["tokens"][x["i"]]["end"]
    x["ast"]["num_names"] += 1


def parse_type_export(ast, tokens, i):
    if tokens[i] != CAPITALISED_NAME_TOKEN:
        return i

    i += 1

    i = eat_up_any_whitespace(tokens, i, ast)

    if tokens[i] != OPEN_PARENTHESIS_TOKEN:
        wrapper_id = get_new_wrapper_id(ast)
        insert_name({"ast": ast, "id": wrapper_id, "tokens": tokens, "i": i})
            
        return i

    i += 1

    i = eat_up_any_whitespace(tokens, i, ast)

    parent_type_id = get_new_parent_type_id(ast)

    while True:
        if tokens[i] == CLOSE_PARENTHESIS_TOKEN:
            return i+1

        ast["parent_type_id"][ast["num_parent_type_id"]] = parent_type_id
        insert_name({"ast": ast, "id": ast["num_custom_type_branch"], "tokens": tokens, "i": i})
        ast["num_custom_type_branch"] += 1

        i += 1


def parse_module_declaration(ast, tokens, i):
    """
    module X exposing (a)

    module X exposing (a, b)

    module X exposing (..)
    """
    try:
        if tokens[i] != MODULE_TOKEN:
            return i
    except IndexError:
        return i

    i += 1

    i = eat_up_any_whitespace(tokens, i)

    module_id = get_new_module_id(ast)
    insert_name({"ast": ast, "id": module_id, "tokens": tokens, "i": i})

    i = eat_up_any_whitespace(tokens, i)

    # assume the next token is "exposing"
    i += 1

    i = eat_up_any_whitespace(tokens, i)

    # assume the next token is "("
    i += 1

    i = eat_up_any_whitespace(tokens, i)

    if tokens[i] == EXPOSE_ALL_TOKEN:
        ast["export_all_module"][ast["num_export_all_module"]] = module_id
        ast["num_export_all_module"] += 1

    else:
        while True:
            j = ast_choice(
                tokens,
                i,
                [parse_type_export, parse_variable_export],
            )

            if j == i:
                break

            i = j

    return i
        


def parse(tokens, src):
    ast = init_ast()

    i = 0
    while i < len(tokens):
        i = ast_choice(
            tokens,
            i,
            [
                parse_module_declaration,
                parse_import,
                parse_line_comment_ast,
                parse_block_comment_ast,
                parse_top_level_function,
                parse_adt_declaration,
                parse_type_alias_declaration,
            ],
        )

    return ast


def read_sources(source_directories):
    src = b""
    for source_directory in source_directories:
        for path in get_elm_paths(os.walk(source_directory)):
            with open(path, "rb") as elm:
                src += elm.read() + bytes([MODULE_SEPARATOR])

    return src


def main():
    with open("elm.json", "rb") as f:
        elm_dot_json = json.load(f)

    src = read_sources(elm_dot_json["source-directories"])

    ast = init_ast()
    parse(tokenize(src), src)


main()
