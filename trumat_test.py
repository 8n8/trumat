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

def test_expression_1():
    input = """module X exposing (x)


x =
    1
"""
    assert trumat.format(input) == input


def test_expression_12():
    input = """module X exposing (x)


x =
    12
"""
    assert trumat.format(input) == input
