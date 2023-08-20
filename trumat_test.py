import trumat

def test_hello_world_formatted():
    input = """module X exposing (x)


x =
    0
"""
    assert trumat.format(input) == input

def test_fix_short_indent():
    input = """module X exposing (x)


x =
   0
"""
    expected = """module X exposing (x)


x =
    0
"""
    assert trumat.format(input) == expected
