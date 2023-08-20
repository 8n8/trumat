import trumat
import hypothesis

def test_basic_function_call():
    input = """module X exposing (x)


x =
    a b
"""
    assert trumat.format(input) == input


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
