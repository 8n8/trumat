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

    return new


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

    return new


def remove_space_before_equals(old):
    new = ""
    for i, c in enumerate(old):
        if c == "=":
            if old[i - 1] == " ":
                new = new[:-1]

        new += c

    return new


def remove_newline_after_equals(old):
    new = ""
    for i, c in enumerate(old):
        if c == "\n":
            if old[i - 1] == "=":
                continue

        new += c

    return new


def remove_space_after_equals(old):
    new = ""
    for i, c in enumerate(old):
        if c == " ":
            if old[i - 1] == "=":
                continue

        new += c

    return new


def remove_space_before_open_bracket(old):
    new = ""
    for i, c in enumerate(old):
        if c == "(":
            if old[i - 1] == " ":
                new = new[:-1]

        new += c

    return new


def remove_space_after_open_list(old):
    new = ""
    for i, c in enumerate(old):
        try:
            if c == ' ' and old[i-1] == '[':
                continue
        except IndexError:
            pass

        new += c

    return new


def remove_space_after_comma(old):
    new = ""
    for i, c in enumerate(old):
        try:
            if c == ' ' and old[i-1] == ',':
                continue

        except IndexError:
            pass

        new += c

    return new


def insert_space_before_equals(old):
    new = ""
    for i, c in enumerate(old):
        if c == "=":
            new += " "

        new += c

    return new


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

    return new


def insert_newlines_before_top_level_bind(old):
    new = ""
    for i, c in enumerate(old):
        new += c

        if c == "\n":
            try:
                if old[i + 1] != " ":
                    new += "\n\n"

            except IndexError:
                continue

    return new


def insert_space_before_open_bracket(old):
    new = ""
    for i, c in enumerate(old):
        if c == "(":
            new += " "

        new += c

    return new


def list_contains_newlines(old, i):
    while i < len(old) and old[i] != ']':
        if old[i] == '\n':
            return True
        i += 1

    return False


def format_list(old, i):
    if old[i] != '[':
        return i, ""
    i += 1 

    new = "["

    has_newlines = list_contains_newlines(old, i)

    while i < len(old) and old[i] != ']':
        i, formatted = format_expression(old, i)
        new += formatted

        if old[i] == "]":
            new += ']'
            break

        if old[i] != '\n':
            new += '\n '

        new += ","

        i += 1

    new += ']'

    return i, new


def is_verbatim_char(char):
    return char in {"a", "b", "c", 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm', 'n', 'o', 'p', 'q', 'r', 's', 't', 'u', 'v', 'w', 'x', 'y', 'z', "A", "B", "C", 'D', 'E', 'F', 'G', 'H', 'I', 'J', 'K', 'L', 'M', 'N', 'O', 'P', 'Q', 'R', 'S', 'T', 'U', 'V', 'W', 'X', 'Y', 'Z', '0', '1', '2', '3', '4', '5', '6', '7', '8', '9', '0', '_'}


def format_verbatim(old, i):
    new = ""
    while i < len(old) and is_verbatim_char(old[i]):
        new += old[i]
        i += 1

    return i, new
        

def format_expression(old, i):
    j, formatted = format_list(old, i)
    if j > i:
        return j, formatted

    return format_verbatim(old, i)


def format_top_level_expressions(old):
    new = ""
    i = 0
    while i < len(old):
        if old[i] == "=":
            if is_toplevel_bind(old, i):
                i, formatted = format_expression(old, i+1)
                new += formatted
                continue

        i += 1

    return new
        

rules = [
    remove_multi_space,
    remove_multi_newline,
    remove_space_before_equals,
    remove_space_after_equals,
    remove_newline_after_equals,
    remove_space_after_equals,
    remove_space_before_open_bracket,
    remove_space_after_open_list,
    remove_space_after_comma,
    format_top_level_expressions,
    insert_space_before_equals,
    insert_whitespace_after_top_level_equals,
    insert_newlines_before_top_level_bind,
    insert_space_before_open_bracket,
]


def format(code):
    for rule in rules:
        code = rule(code)

    return code
