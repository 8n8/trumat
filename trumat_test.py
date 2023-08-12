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


@given(
    spaces=st.integers(min_value=0, max_value=10),
    expression=st.integers())
def test_spaces_before_top_bind_equals(spaces, expression):
    input = f"""module X exposing (x)


x{spaces * " "}=
    {expression}
"""
    expected = f"""module X exposing (x)


x =
    {expression}
"""
    assert trumat.format(input) == expected


@given(expression=st.integers())
def test_multiple_top_binds(expression):
    input = f"""module X exposing (x)


x =
    0


y =
    {expression}
"""
    expected = f"""module X exposing (x)


x =
    0


y =
    {expression}
"""
    assert trumat.format(input) == expected

@st.composite
def generate_lower_name(draw):
    first_char = draw(st.text(alphabet="abcdefghijklmnopqrstuvwxyz_", min_size=1, max_size=1))
    subsequent = draw(st.text(alphabet="abcdefghijklmnopqrstuvwxyz_ABCDEFGHIJKLMNOPQRSTUVWXYZ"))
    return first_char + subsequent

@given(name=generate_lower_name())
def test_random_bind_name(name):
    input = f"""module X exposing (x)


{name} =
    0
"""
    assert trumat.format(input) == input

@given(
    spaces1=st.integers(min_value=0, max_value=10),
    num_newlines=st.integers(min_value=0, max_value=10),
    spaces2=st.integers(min_value=1, max_value=10))
def test_newlines_after_top_bind_name(spaces1, num_newlines, spaces2):
    newlines = '\n' * num_newlines
    input = f"""module X exposing (x)


x{spaces1 * " "}{newlines}{spaces2 * " "}= 0
"""
    expected = """module X exposing (x)


x =
    0
"""
    assert trumat.format(input) == expected


@given(
    text=st.text(alphabet="abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ`0123456789!£$%^&*()-_=+[]{};'#:@~,./<>?"))
def test_simple_string_literal(text):
    input = f"""module X exposing (x)


x =
    "{text}"
"""
    assert trumat.format(input) == input
