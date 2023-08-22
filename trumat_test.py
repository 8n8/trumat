import trumat
import hypothesis

def test_hello_world():
    input = """module X exposing (x)


x =
    0
"""
    assert trumat.format(input) == input
