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

    return new


def insert_verbatims(old):
    new = ""
    index = 0
    for c in old:
        if c == "X":
            new += VERBATIMS[index]
            index += 1
            continue

        new += c

    return new


def list_has_newlines(old, i):
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


def mark_newline_lists(old):
    new = ""
    for i, c in enumerate(old):
        if c == "[" and list_has_newlines(old, i + 1):
            new += "L"
            continue

        new += c

    return new


def unmark_newline_lists(old):
    new = ""
    for c in old:
        if c == "L":
            new += "["
            continue

        new += c

    return new


def debug(old):
    print(old)
    return old


def remove_space_after_L(old):
    new = ""

    for i, c in enumerate(old):
        if c == " ":
            try:
                if old[i - 1] == "L":
                    continue
            except IndexError:
                pass

        new += c

    return new


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

    return new


def remove_space_after_comma(old):
    new = ""

    for i, c in enumerate(old):
        if c == " ":
            try:
                if old[i - 1] == ",":
                    continue

            except IndexError:
                pass

        new += c

    return new


def remove_space_before_close_bracket(old):
    new = ""

    for i, c in enumerate(old):
        if c == " ":
            try:
                if old[i + 1] == "]":
                    continue

            except IndexError:
                pass

        new += c

    return new


rules = [
    remove_verbatims,
    mark_newline_lists,
    remove_multi_space,
    remove_multi_newline,
    remove_space_before_equals,
    remove_space_after_equals,
    remove_newline_after_equals,
    remove_space_after_equals,
    remove_space_before_open_bracket,
    remove_space_after_L,
    remove_newline_before_space,
    remove_space_after_comma,
    remove_space_before_close_bracket,
    debug,
    insert_space_before_equals,
    insert_whitespace_after_top_level_equals,
    insert_newlines_before_top_level_bind,
    insert_space_before_open_bracket,
    unmark_newline_lists,
    insert_verbatims,
]


def format(code):
    global VERBATIMS
    VERBATIMS = []
    for rule in rules:
        code = rule(code)

    return code
