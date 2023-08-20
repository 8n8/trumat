import trumat
import hypothesis

@hypothesis.given(
    first_char=hypothesis.strategies.text(alphabet="abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ_", min_size=1, max_size=1),
    remaining_chars=hypothesis.strategies.text(alphabet="abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ_0123456789"))
def test_function_call_with_trailing(first_char, remaining_chars):
    name = first_char + remaining_chars
    input = f"""module X exposing (x)


x =
    {name} b
"""
    assert trumat.format(input) == input

@hypothesis.given(infix_spaces=hypothesis.strategies.text(alphabet=" "))
def test_function_call_with_variable_infix_spaces(infix_spaces):
    input = f"""module X exposing (x)


x =
    a{infix_spaces} b
"""
    expected = f"""module X exposing (x)


x =
    a b
"""
    assert trumat.format(input) == expected


@hypothesis.given(
    integer=hypothesis.strategies.integers(min_value=0),
    spaces=hypothesis.strategies.text(alphabet=" "))
def test_positive_integer_literal_with_trailing(integer, spaces):
    input = f"""module X exposing (x)


x =
    {integer}{spaces}
"""
    expected = f"""module X exposing (x)


x =
    {integer}
"""
    assert trumat.format(input) == expected
