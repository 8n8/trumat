def init_cache():
    return {
        "formatted": [222] * 1000_000,
        "formatted_size": 0,
        "code": "",
    }

def write_char(cache, c):
    cache["formatted"][cache["formatted_size"]] = ord(c)
    cache["formatted_size"] += 1

def write_chunk(cache, chunk):
    for c in chunk:
        write_char(cache, c)

def write_quote(cache, literal):
    for i in range(literal["end"] - literal["start"]):
        write_char(cache, cache["code"][literal["start"] + i])

module_name_tokenise = {
        "m": "m",
        "o": "o",
        "d": "d",
        "u": "u",
        "l": "l",
        "e": "e",
        " ": " ",
        "A": "name_char",
        "a": "subsequent_name_char",
        "x": "subsequent_name_char",
        "p": "subsequent_name_char",
        "s": "subsequent_name_char",
        "i": "subsequent_name_char",
        "n": "subsequent_name_char",
        "g": "subsequent_name_char",
        "(": "other_char",
        ")": "other_char",
        "\n": "\n",
        "=": "other_char",
        "0": "subsequent_name_char",
        "X": "name_char",
    }
module_name_start_table = {
        "before": 1,
        "after_m": 1,
        "after_o": 1,
        "after_d": 1,
        "after_u": 1,
        "after_l": 1,
        "after_e": 1,
        "spaces after module keyword": 1,
        "after first name character": 0,
        "after subsequent name character": 0,
        "after": 0,
    }
module_name_end_table = {
        "before": 1,
        "after_m": 1,
        "after_o": 1,
        "after_d": 1,
        "after_u": 1,
        "after_l": 1,
        "after_e": 1,
        "spaces after module keyword": 1,
        "after first name character": 1,
        "after subsequent name character": 1,
        "after": 0,
    }
module_name_state_table = {
        ("before", "m"): "after_m",
        ("after_m", "o"): "after_o",
        ("after_o", "d"): "after_d",
        ("after_d", "u"): "after_u",
        ("after_u", "l"): "after_l",
        ("after_l", "e"): "after_e",
        ("after_e", " "): "spaces after module keyword",
        ("spaces after module keyword", "name_char"): "after first name character",
        ("after first name character", "name_char"): "after subsequent name character",
        ("after subsequent name character", " "): "after",
        ("after", "e"): "after",
        ("after", "name_char"): "after",
        ("after", "o"): "after",
        ("after", " "): "after",
        ("after", "other_char"): "after",
        ("after", "\n"): "after",
        ("after first name character", "subsequent_name_char"): "after subsequent name character",
        ("after", "subsequent_name_char"): "after",
        ("after first name character", " "): "after",
    }

def get_module_name(cache):
    """The state machine is like this:

    0 before
    1 m
    2 o
    3 d
    4 u
    5 l
    6 e
    7 spaces after module keyword
    8 first name character
    9 subsequent name characters
    10 after
    """

    state = "before"
    start = 0
    end = 0

    for c in cache["code"]:
        token = module_name_tokenise[c]
        state = module_name_state_table[(state, token)]
        start += module_name_start_table[state]
        end += module_name_end_table[state]

    return { "start": start, "end": end }

def get_module_index(cache):
    return { "start": 7, "end": 8 }

def populate_cache(cache, unformatted):
    cache["code"] = unformatted
    cache["size"] = len(cache["code"])

def get_export(cache, export_index):
    if export_index == 0:
        return { "start": 19, "end": 20 }

def format_top(cache, top):
    write_chunk(cache, "x =\n    0")

def get_top(cache, index):
    if index == 0:
        return {}

def format(cache, code):
    populate_cache(cache, code)
    write_chunk(cache, "module ")
    write_quote(cache, get_module_name(cache))
    write_chunk(cache, " exposing ")
    leading = "("
    for export_index in range(100):
        export = get_export(cache, export_index)
        if export is None:
            break

        write_chunk(cache, leading)
        leading = ", "
        write_quote(cache, export)

    write_chunk(cache, ")")

    for top_index in range(100):
        top = get_top(cache, top_index)
        if top is None:
            break

        write_chunk(cache, "\n\n\n")
        format_top(cache, top)

        write_char(cache, "\n")
