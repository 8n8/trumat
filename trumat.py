RAW = ""
INDEX = 0


def forward():
    global INDEX
    INDEX += 1


def parse_chunk(chunk):
    for chunk_character in chunk:
        if RAW[INDEX] != chunk_character:
            raise ValueError(f"expecting {chunk_character} but got {RAW[INDEX]} at position {INDEX} in source while parsing chunk {chunk}")
        forward()

def initialise_globals(unformatted):
    global RAW
    RAW = unformatted

    global INDEX
    INDEX = 0

def some(parser):
    results = []
    while True:
        try:
            results.append(parser())

        except (ValueError, IndexError):
            break

    if len(results) == 0:
        raise ValueError("combinator 'some' expects the parser to pass at least once")

    return results

def parse_any_char():
    char = RAW[INDEX]
    forward()
    return char

def parse_whitespace():
    try:
        while RAW[INDEX] in "\n ":
            forward()
    except IndexError:
        pass

def parse_int():
    start = INDEX
    while RAW[INDEX] in "-0123456789":
        forward()

    return RAW[start:INDEX]

def parse_name():
    start = INDEX
    try:
        while RAW[INDEX] not in " =":
            forward()
    except IndexError:
        pass
    return RAW[start:INDEX]

def parse_top_level():
    parse_whitespace()
    name = parse_name()
    print("HERE")
    parse_whitespace()
    parse_chunk("=")
    parse_whitespace()
    expression = parse_int()
    return f"""{name} =
    {expression}"""

def format(unformatted):
    print(unformatted)
    initialise_globals(unformatted)
    parse_chunk("""module X exposing (x)


""")
    top_levels = "\n\n\n".join(some(parse_top_level))
    return f"""module X exposing (x)


{top_levels}
"""
