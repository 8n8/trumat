import trumat
import hypothesis

def test_hello_world_unformatted():
    input = """module X exposing (x)


x =
   0
"""
    expected = """module X exposing (x)


x =
    0
"""
    assert trumat.format(input) == expected

def test_hello_world():
    input = """module X exposing (x)


x =
    0
"""
    assert trumat.format(input) == input
