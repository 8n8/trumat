import trumat

def to_string(cache):
    s = ""
    for i in range(cache["formatted_size"]):
        s += chr(cache["formatted"][i])

    return s


def test_hello_world_formatted():
    input = """module X exposing (x)


x =
    0
"""
    cache = trumat.init_cache()
    trumat.format(cache, input)
    assert to_string(cache) == input
