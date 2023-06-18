import trumat


def test_hello_world():
    hello_world = """module X exposing (x)


x =
    0
"""
    got = trumat.format(hello_world)
    assert got == hello_world
