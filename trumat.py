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

def get_module_name(cache):
    return { "start": 7, "end": 8 }

def module_map(c):
    if c == "m":
        return 6
    if c == "o":
        return 5
    if c == "d":
        return 4
    if c == "u":
        return 3
    if c == "l":
        return 2
    if c == "e":
        return 1
    return 0

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
