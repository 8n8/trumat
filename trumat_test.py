import trumat
import hypothesis

@hypothesis.given(
    trailing_spaces=hypothesis.strategies.text(alphabet=" "),
    infix_spaces=hypothesis.strategies.text(alphabet=" "))
def test_basic_function_call(trailing_spaces, infix_spaces):
    input = f"""module X exposing (x)


x =
    a {infix_spaces}b{trailing_spaces}
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
