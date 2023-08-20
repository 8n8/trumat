import trumat
import hypothesis

@hypothesis.strategies.composite
def name_gen(draw):
    first_char = draw(hypothesis.strategies.text(alphabet="abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ_", min_size=1, max_size=1))
    remaining_chars = draw(hypothesis.strategies.text(alphabet="abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ_0123456789"))
    return first_char + remaining_chars

@hypothesis.given(name=name_gen())
def test_function_call(name):
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
