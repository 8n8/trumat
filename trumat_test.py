from hypothesis import given, strategies as st
import trumat

def test_hello_world():
    input = """module X exposing (x)


x =
    0
"""
    assert trumat.format(input) == input


def test_silly():
    assert 1 == 1


@given(x=st.integers(), y=st.integers())
def test_ints_are_commutative(x, y):
    assert x + y == y + x
