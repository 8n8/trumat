def remove_multi_space(old):
    count = 0
    new = ""
    for c in old:
        if c == " ":
            count += 1

        else:
            count = 0

        if count > 1:
            continue

        new += c

    return new, None


def remove_multi_newline(old):
    count = 0
    new = ""
    for c in old:
        if c == "\n":
            count += 1

        else:
            count = 0

        if count > 1:
            continue

        new += c

    return new, None


def is_toplevel_bind(old, i):
    for j in range(i):
        if old[i - j] == "\n":
            return old[i - j + 1] != " "


def insert_whitespace_after_top_level_equals(old):
    new = ""
    for i, c in enumerate(old):
        new += c

        if c == "=":
            if is_toplevel_bind(old, i):
                new += "\n    "

    return new, None


def insert_newlines_before_top_level_bind(old):
    new = ""
    for i, c in enumerate(old):
        new += c

        if c == "\n":
            try:
                if old[i + 1] != " " and old[i + 1] != "\n":
                    new += "\n\n"

            except IndexError:
                continue

    return new, None


VERBATIMS = []


keywords = {"module", "exposing", "if", "then", "else"}


def remove_verbatim(i, old):
    if not is_verbatim_char(old[i]):
        return i

    try:
        if is_verbatim_char(old[i - 1]):
            return i

    except IndexError:
        pass

    j = i + 0
    verbatim = ""
    while is_verbatim_char(old[j]):
        verbatim += old[j]
        j += 1

    if verbatim in keywords:
        return i

    VERBATIMS.append(verbatim)

    return j


def is_verbatim_char(ch):
    return ch not in {"\n", "(", ")", "[", "]", "=", " ", ","}


def remove_verbatims(old):
    new = ""

    i = 0
    while i < len(old):
        j = remove_verbatim(i, old)
        if j > i:
            new += "X"
            i = j
            continue

        new += old[i]
        i += 1

    return new, None


def insert_verbatims(old):
    new = ""
    index = 0
    for c in old:
        if c == "X":
            new += VERBATIMS[index]
            index += 1
            continue

        new += c

    return new, None


def list_has_newlines_forward(old, i):
    level = 1

    while level != 0:
        if old[i] == "\n":
            return True

        if old[i] == "[":
            level += 1

        if old[i] == "]":
            level -= 1

        i += 1

    return False


def list_has_newlines_backward(old, i):
    level = 1

    while level != 0:
        if old[i] == "L":
            return True

        if old[i] == "]":
            level += 1

        if old[i] == "M":
            level += 1

        if old[i] == "[":
            level -= 1

        i -= 1

    return False


def list_is_empty_forward(old, i):
    while i < len(old) and (old[i] == " " or old[i] == "\n"):
        i += 1

    return old[i] == "]"


def mark_start_newline_lists(old):
    new = ""
    for i, c in enumerate(old):
        if (
            c == "["
            and list_has_newlines_forward(old, i + 1)
            and not list_is_empty_forward(old, i + 1)
        ):

            new += "L"
            continue

        new += c

    return new, None


def mark_end_newline_lists(old):
    new = ""
    for i, c in enumerate(old):
        if c == "]" and list_has_newlines_backward(old, i - 1):
            new += "M"
            continue

        new += c

    return new, None


def debug(old):
    print(old)
    return old, None


def mark_newline_commas(old):
    new = ""
    level = 0

    for i, c in enumerate(old):
        if c == "L":
            level += 1

        if c == "," and level > 0:
            new += "C"
            continue

        if c == "]":
            level -= 1

        new += c

    return new, None


def format_newline_list(old, i, indent):
    formatted = ""

    if old[i] != "L":
        return "", i, None

    formatted = "L"
    i += 1

    while old[i] != "M":
        formatted_sub, i, err = format_expression(old, i, indent + 2)
        if err is not None:
            return "", i, err

        formatted += formatted_sub

        if old[i] == "C":
            formatted += "\n" + indent * " " + "C"
            i += 1
            continue

        if old[i] != "C" and old[i] != "M":
            return "", i, f"expecting a comma or M but got {old[i]}"

    formatted += "\n" + indent * " " + "M"

    return formatted, i + 1, None


def format_verbatim(old, i, indent):
    if old[i] != "X" and old[i] != "S":
        return "", i

    return old[i], i + 1


def format_if_then_else(old, i, indent):
    new = ""
    if old[i] != "I":
        return "", i, None

    i += 1
    new += "I"

    if_, i, err = format_expression(old, i, indent)
    if err is not None:
        return "", i, err
    new += if_

    if old[i] != "T":
        return "", i, f"expecting T but got {old[i]}"

    i += 1
    indent += 4
    new += "T\n" + indent * " "

    then_, i, err = format_expression(old, i, indent)
    if err is not None:
        return "", i, err
    new += then_

    if old[i] != "E":
        return "", i, f"expecting E but got {old[i]}"

    i += 1
    new += "\n\n" + (indent - 4) * " " + "E\n" + indent * " "

    else_, i, err = format_expression(old, i, indent)
    if err is not None:
        return "", i, err
    new += else_

    return new, i, None


def format_expression(old, i, indent):
    formatted, j = format_verbatim(old, i, indent)
    if j > i:
        return formatted, j, None

    formatted, j, err = format_if_then_else(old, i, indent)
    if j > i:
        return formatted, j, err

    return format_newline_list(old, i, indent)


def format_top_level_expressions(old):
    new = ""

    i = 0
    while i < len(old):
        new += old[i]
        if old[i] == "=":
            formatted, i, err = format_expression(old, i + 1, 4)
            if err is not None:
                return "", (i, err)

            new += formatted
            continue

        i += 1

    return new, None


def mark_empty_lists(old):
    new = ""

    i = 0
    while i < len(old):
        if old[i] == "[":
            j = i + 1
            try:
                while old[j] == " " or old[j] == "\n":
                    j += 1

                if old[j] == "]":
                    new += "S"
                    j += 1
                    i = j
                    continue

            except IndexError:
                pass

        new += old[i]
        i += 1

    return new, None


def remove_first_char(chars):
    def helper(old):
        new = ""

        for i, c in enumerate(old):
            if c == chars[0]:
                try:
                    if old[i + 1] == chars[1]:
                        continue

                except IndexError:
                    pass

            new += c

        return new, None

    return helper


def remove_second_char(chars):
    def helper(old):
        new = ""

        for i, c in enumerate(old):
            if c == chars[1]:
                try:
                    if old[i - 1] == chars[0]:
                        continue

                except IndexError:
                    pass

            new += c

        return new, None

    return helper


def replace_char_with(a, b):
    def helper(old):
        new = ""

        for c in old:
            if c == a:
                new += b
                continue

            new += c

        return new, None

    return helper


def is_language_character(char):
    return char in " \n"


def replace_keyword(keyword, replacement):
    def helper(old):
        new = ""

        i = 0
        while i < len(old):
            try:
                if (
                    is_language_character(old[i - 1])
                    and old[i : i + len(keyword)] == keyword
                    and is_language_character(old[i + len(keyword)])
                ):

                    new += replacement
                    i += len(keyword)
                    continue

            except IndexError:
                pass

            new += old[i]
            i += 1

        return new, None

    return helper


rules = [
    remove_verbatims,
    mark_start_newline_lists,
    mark_end_newline_lists,
    mark_empty_lists,
    mark_newline_commas,
    replace_keyword("if", "I"),
    replace_keyword("then", "T"),
    replace_keyword("else", "E"),
    remove_multi_space,
    remove_multi_newline,
    remove_first_char(" ="),
    remove_second_char("=\n"),
    remove_second_char("= "),
    remove_first_char(" ("),
    remove_second_char("L "),
    remove_second_char("[ "),
    remove_first_char("\n "),
    remove_first_char("\nM"),
    remove_second_char("C "),
    remove_second_char(", "),
    remove_first_char(" C"),
    remove_first_char(" ]"),
    remove_first_char(" M"),
    remove_second_char("I "),
    remove_first_char(" T"),
    remove_second_char("T "),
    remove_first_char(" E"),
    remove_second_char("E "),
    format_top_level_expressions,
    replace_char_with("=", " ="),
    insert_whitespace_after_top_level_equals,
    insert_newlines_before_top_level_bind,
    replace_char_with("I", "I "),
    replace_char_with("T", " T"),
    replace_char_with("(", " ("),
    replace_char_with("]", " ]"),
    replace_char_with("C", ","),
    replace_char_with("[", "[ "),
    replace_char_with("L", "L "),
    replace_char_with("L", "["),
    replace_char_with("M", "]"),
    replace_char_with("S", "[]"),
    replace_char_with(",", ", "),
    replace_char_with("I", "if"),
    replace_char_with("T", "then"),
    replace_char_with("E", "else"),
    insert_verbatims,
]


def format(code):
    global VERBATIMS
    VERBATIMS = []
    for rule in rules:
        code, err = rule(code)
        if err is not None:
            return "", err

    return code, None
