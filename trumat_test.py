import trumat

def to_string(cache):
    s = ""
    for i in range(cache["formatted_size"]):
        s += chr(cache["formatted"][i])

    return s


def _test_helper(unformatted, formatted):
    cache = trumat.init_cache()
    trumat.format(cache, unformatted)
    return to_string(cache)


def test_hello_world_formatted():
    input = """module X exposing (x)


x =
    0
"""
    assert _test_helper(input, input)
