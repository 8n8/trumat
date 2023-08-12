from hypothesis import given, settings, strategies as st
import trumat

formatteds = [
    """module X exposing (x)


x =
    0
"""
]


def test_formatted():
    for formatted in formatteds:
        assert trumat.format(formatted) == formatted


unformatteds = [
    {
        "input": """module X exposing (x)


x =
   0
""",
        "expected": """module X exposing (x)


x =
    0
""",
    }
]


def test_unformatted():
    for unformatted in unformatteds:
        assert trumat.format(unformatted["input"]) == unformatted["expected"]


@given(x=st.integers(min_value=0))
def test_random_int_value(x):
    input = f"""module X exposing (x)


x =
    {x}
"""
    assert trumat.format(input) == input


@given(
    indent=st.integers(min_value=0, max_value=200),
    value=st.integers(min_value=0)
    )
def test_random_top_level_indent(indent, value):
    input = f"""module X exposing (x)


x =
 {' ' * indent}{value}
"""
    expected = f"""module X exposing (x)


x =
    {value}
"""
    assert trumat.format(input) == expected
