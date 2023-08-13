"""This contains the unformatted code."""
RAW = ""
"""This is the position of the formatter in the unformatted code."""
INDEX = 0


def next():
    """ Move the global position forward. """
    global INDEX
    INDEX += 1


def parse_chunk(chunk):
    """ It will succeed if the next piece of the unformatted code matches
    the given chunk of text and fail if it doesn't.
    """
    for chunk_character in chunk:
        if RAW[INDEX] != chunk_character:
            raise ValueError(f"expecting {chunk_character} but got {RAW[INDEX]} at position {INDEX} in source while parsing chunk {chunk}")
        next()

def initialise_globals(unformatted):
    """ Initialise the global variables. It's necessary because the
    formatter gets run many times in the tests. You need to reset the
    global variables each time.
    """
    global RAW
    RAW = unformatted

    global INDEX
    INDEX = 0

def some(parser):
    """Run the parser provided one or more times and collect the results
    in a list. It will fail if the provided parser doesn't succeed at least
    once."""
    results = []
    while True:
        try:
            results.append(parser())

        except (ValueError, IndexError):
            break

    if not results:
        raise ValueError("combinator 'some' expects the parser to pass at least once")

    return results

def parse_whitespace():
    """ Consome zero or more white space characters. """
    try:
        while RAW[INDEX] in "\n ":
            next()
    except IndexError:
        pass

def parse_number():
    """ Parse an integer literal. """
    start = INDEX
    while RAW[INDEX] in "abcdef0123456789x-.":
        next()
    
    return RAW[start:INDEX]

def parse_lower_name():
    """ Parse a lower-case name, like aBc_ """
    start = INDEX
    try:
        while RAW[INDEX] not in " =\n":
            next()
    except IndexError:
        pass
    return RAW[start:INDEX]

def one_of(*parsers):
    for parser in parsers:
        try:
            return parser()
        except (ValueError, IndexError):
            pass

    raise ValueError("one_of expects that one of the parsers will succeed")

def parse_simple_string_literal():
    start = INDEX
    parse_chunk('"')
    while RAW[INDEX] != '"':
        if RAW[INDEX:INDEX+2] == '\\\"':
            next()

        if RAW[INDEX:INDEX+2] == '\\\\':
            next()

        next()

    next()

    return RAW[start:INDEX]

def parse_triple_string_literal():
    start = INDEX
    if RAW[INDEX:INDEX+3] != '"""':
        raise ValueError('expecting """ at start of triple string literal')

    next()
    next()
    next()

    while RAW[INDEX:INDEX+2] != '""':
        if RAW[INDEX] == '\\':
            next()

        next()

    next()
    next()
    next()
    return RAW[start:INDEX]

def parse_expression():
    """Parse an Elm expression, like 0 or "hello" or List.reverse [1, 2]"""
    return one_of(
        parse_triple_string_literal,
        parse_simple_string_literal,
        parse_number)

def parse_top_level():
    """Parse a top-level item in the file, such as a function declaration,
    a type declaration or a block comment. """
    parse_whitespace()
    name = parse_lower_name()
    parse_whitespace()
    parse_chunk("=")
    parse_whitespace()
    expression = parse_expression()
    return f"""{name} =
    {expression}"""

def format(unformatted):
    """ Format the provided code according to elm-format standards. """
    initialise_globals(unformatted)
    parse_chunk("""module X exposing (x)


""")
    top_levels = "\n\n\n".join(some(parse_top_level))
    return f"""module X exposing (x)


{top_levels}
"""
