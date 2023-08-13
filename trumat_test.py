from hypothesis import assume, given, settings, strategies as st
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

@st.composite
def generate_simple_string_literal(draw):
    contents = ""
    length = draw(st.integers(min_value=0, max_value=20))
    for _ in range(length):
        item = draw(
                st.one_of(
                    st.text(alphabet="abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ`0123456789!£$%^&*()-_=+[]{};'#:@~,./<>?"),
                    st.just('\\\\'),
                    st.just('\\n'),
                    st.just('\\t'),
                    st.just('\\u'),
                    st.just('\\\"')))
        contents += item

    return f'"{contents}"'

@given(text=generate_simple_string_literal())
def test_simple_string_literal_with_escaped_quote(text):
    input = f"""module X exposing (x)


x =
    {text}
"""
    assert trumat.format(input) == input


@given(
    sign=st.one_of(st.just(""), st.just("-")),
    afterX=st.text(alphabet="abcdef0123456789", min_size=1))
def test_hex_literal(sign, afterX):
    input = f"""module X exposing (x)


x =
    {sign}0x{afterX}
"""
    assert trumat.format(input) == input


@st.composite
def generate_float_literal(draw):
    sign = draw(st.one_of(st.just(""), st.just("-")))
    first_digit = draw(st.text(alphabet="0123456789", min_size=1, max_size=1))
    more_digits = draw(st.text(alphabet="0123456789", min_size=1))
    exp = draw(st.one_of(st.just("e")), st.just(""))
    after_exp = draw(st.text(alphabet="-0123456789"))
    assume(exp + after_exp != "e")
    assume(exp + after_exp != "e-")
    return f"{sign}{first_digit}.{more_digits}{exp}{after_exp}"

@given(float_literal=generate_float_literal())
def test_float_literal(float_literal):
    input = f"""module X exposing (x)


x =
    {float_literal}
"""
    assert trumat.format(input) == input

@st.composite
def generate_simple_triple_string_literal(draw):
    contents = draw(st.text(alphabet="abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ`0123456789!£$%^&*()-_=+[]{};'#:@~,./<>?\n\""))
    try:
        assume(contents[-1] != '"' or contents[-2:] == '\\"')
    except IndexError:
        pass
    extras = draw(st.sampled_from(["", "\\n", "\\\\", "\\t"]))
    assume('"""' not in contents)
    return extras + contents

@given(triple_string_literal=generate_simple_triple_string_literal())
def test_triple_string_literal(triple_string_literal):
    input = f"""module X exposing (x)


x =
    \"\"\"{triple_string_literal}\"\"\"
"""
    assert trumat.format(input) == input
