import trumat
import hypothesis

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

def test_int_trailing_space():
    input = """module X exposing (x)


x =
    0 
"""
    expected = """module X exposing (x)


x =
    0
"""
    assert trumat.format(input) == expected

def test_basic_function_call():
    input = """module X exposing (x)


x =
    a b
"""
    assert trumat.format(input) == input

def test_trailing_space_and_non_zero_start1():
    input = """module X exposing (x)


x =
    1 
"""
    expected = """module X exposing (x)


x =
    1
"""
    assert trumat.format(input) == expected

@hypothesis.given(integer=hypothesis.strategies.integers(min_value=0))
def test_positive_integer_literal_with_trailing(integer):
    input = f"""module X exposing (x)


x =
    {integer} 
"""
    expected = f"""module X exposing (x)


x =
    {integer}
"""
    assert trumat.format(input) == expected
