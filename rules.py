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


def remove_space_before_equals(old):
    new = ""
    for i, c in enumerate(old):
        if c == "=":
            if old[i - 1] == " ":
                new = new[:-1]

        new += c

    return new, None


def remove_newline_after_equals(old):
    new = ""
    for i, c in enumerate(old):
        if c == "\n":
            if old[i - 1] == "=":
                continue

        new += c

    return new, None


def remove_space_after_equals(old):
    new = ""
    for i, c in enumerate(old):
        if c == " ":
            if old[i - 1] == "=":
                continue

        new += c

    return new, None


def remove_space_before_open_bracket(old):
    new = ""
    for i, c in enumerate(old):
        if c == "(":
            if old[i - 1] == " ":
                new = new[:-1]

        new += c

    return new, None


def insert_space_before_equals(old):
    new = ""
    for i, c in enumerate(old):
        if c == "=":
            new += " "

        new += c

    return new, None


def insert_space_after_comma(old):
    new = ""
    for c in old:
        if c == ",":
            new += ", "
            continue

        new += c

    return new, None


def insert_space_after_start_list(old):
    new = ""
    for i, c in enumerate(old):
        if c == "[":
            try:
                if old[i + 1] != "]":
                    new += "[ "
                    continue

            except IndexError:
                pass

        new += c

    return new, None


def insert_space_before_close_ordinary_list(old):
    new = ""
    for i, c in enumerate(old):
        if c == "]":
            try:
                if old[i - 1] != "[":
                    new += " ]"
                    continue

            except IndexError:
                pass

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
                if old[i + 1] != " ":
                    new += "\n\n"

            except IndexError:
                continue

    return new, None


def insert_space_before_open_bracket(old):
    new = ""
    for i, c in enumerate(old):
        if c == "(":
            new += " "

        new += c

    return new, None


VERBATIMS = []


keywords = {"module", "exposing"}


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


def unmark_start_newline_lists(old):
    new = ""
    for c in old:
        if c == "L":
            new += "["
            continue

        new += c

    return new, None


def unmark_end_newline_lists(old):
    new = ""
    for c in old:
        if c == "M":
            new += "]"
            continue

        new += c

    return new, None


def debug(old):
    print(old)
    return old, None


def remove_space_after_open_list(old):
    new = ""

    for i, c in enumerate(old):
        if c == " ":
            try:
                if old[i - 1] == "L" or old[i - 1] == "[":
                    continue
            except IndexError:
                pass

        new += c

    return new, None


def remove_newline_before_space(old):
    new = ""

    for i, c in enumerate(old):
        if c == "\n":
            try:
                if old[i + 1] == " ":
                    continue

            except IndexError:
                pass

        new += c

    return new, None


def remove_space_after_comma(old):
    new = ""

    for i, c in enumerate(old):
        if c == " ":
            try:
                if old[i - 1] == "C" or old[i - 1] == ",":
                    continue

            except IndexError:
                pass

        new += c

    return new, None


def remove_space_before_close_bracket(old):
    new = ""

    for i, c in enumerate(old):
        if c == " ":
            try:
                if old[i + 1] == "]" or old[i + 1] == "M":
                    continue

            except IndexError:
                pass

        new += c

    return new, None


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


def unmark_newline_commas(old):
    new = ""

    for i, c in enumerate(old):
        if c == "C":
            new += ","
            continue

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
    if old[i] != "X" and old[i] != "E":
        return "", i

    return old[i], i + 1


def format_expression(old, i, indent):
    formatted, j = format_verbatim(old, i, indent)
    if j > i:
        return formatted, j, None

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


def remove_space_before_comma(old):
    new = ""

    for i, c in enumerate(old):
        if c == " ":
            try:
                if old[i + 1] == "C":
                    continue

            except IndexError:
                pass

        new += c

    return new, None


def remove_newline_before_close_bracket(old):
    new = ""
    for i, c in enumerate(old):
        if c == "\n":
            try:
                if old[i + 1] == "M":
                    continue

            except IndexError:
                pass

        new += c

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
                    new += "E"
                    j += 1
                    i = j
                    continue

            except IndexError:
                pass

        new += old[i]
        i += 1

    return new, None


def unmark_empty_lists(old):
    new = ""

    for c in old:
        if c == "E":
            new += "[]"
            continue

        new += c

    return new, None


rules = [
    remove_verbatims,
    mark_start_newline_lists,
    mark_end_newline_lists,
    mark_empty_lists,
    mark_newline_commas,
    remove_multi_space,
    remove_multi_newline,
    remove_space_before_equals,
    remove_space_after_equals,
    remove_newline_after_equals,
    remove_space_after_equals,
    remove_space_before_open_bracket,
    remove_space_after_open_list,
    remove_newline_before_space,
    remove_newline_before_close_bracket,
    remove_space_after_comma,
    remove_space_before_comma,
    remove_space_before_close_bracket,
    format_top_level_expressions,
    insert_space_before_equals,
    insert_whitespace_after_top_level_equals,
    insert_newlines_before_top_level_bind,
    insert_space_before_open_bracket,
    insert_space_before_close_ordinary_list,
    unmark_newline_commas,
    unmark_start_newline_lists,
    unmark_end_newline_lists,
    unmark_empty_lists,
    insert_space_after_comma,
    insert_space_after_start_list,
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
