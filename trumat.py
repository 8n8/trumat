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

    0 after spaces after module keyword
    1 first name character
    2 subsequent name characters
    3 after
    """

    state = "before"
    if cache["module_keyword_index"] is None:
        return
    start = cache["module_keyword_index"]
    end = 0

    for c in cache["code"]:
        token = module_name_tokenise[c]
        state = module_name_state_table[(state, token)]
        start += module_name_start_table[state]
        end += module_name_end_table[state]

    return { "start": start, "end": end }

def get_module_index(cache):
    return { "start": 7, "end": 8 }

def index_exports(cache):
    """ The states are:

    - after module keyword
    - inside export list
    - after export list
    """

    return

index_module_keyword_tokenizer = {
    "m": "m",
    "o": "o",
    "d": "d",
    "u": "u",
    "l": "l",
    "e": "e",
    " ": " ",
    "A": "other",
    "a": "other",
    'x': 'other',
    'p': 'other',
    's': 'other',
    'i': 'other',
    'n': 'other',
    'g': 'other',
    '(': 'other',
    ')': 'other',
    '\n': ' ',
    '=': 'other',
    '0': 'other',
}

index_module_keyword_transition = {
    ("start", "m"): "after m",
    ("after m", "o"): "after o",
    ("after o", "d"): "after d",
    ("after d", "u"): "after u",
    ("after u", "l"): "after l",
    ("after l", "e"): "after e",
    ("after e", " "): "just after module keyword",
    ("just after module keyword", "other"): "after",
    ("after", "other"): "after",
    ('after', ' '): 'after',
    ('after', 'e'): 'after',
    ('after', 'o'): 'after',
}

index_module_keyword_start = {
    "after m": 1,
    "after o": 1,
    "after d": 1,
    "after u": 1,
    "after l": 1,
    "after e": 1,
    "just after module keyword": -6,
    "after": 0,
}

def index_module_keyword(code):
    state = "start"
    start = 0

    for c in code:
        token = index_module_keyword_tokenizer[c]
        state = index_module_keyword_transition[(state, token)]
        start += index_module_keyword_start[state]

    return start

def populate_cache(cache, unformatted):
    cache["code"] = unformatted
    cache["size"] = len(cache["code"])

    cache['module_keyword_index'] = index_module_keyword(cache['code'])
    index_exports(cache)

def get_export(cache, export_index):
    if export_index >= cache["num_exports"]:
        return

    return {"start": cache["export_start"][export_index],
            "end": cache["export_end"][export_index]}

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
