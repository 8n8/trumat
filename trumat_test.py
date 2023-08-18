import trumat

formatteds = [
    """module X exposing (x)


x =
    0
"""
]

unformatteds = [
    ( """module X exposing (x)


x =
   0
""",
    """module X exposing (x)


x =
    0
""")
]


def test_formatted():
    for formatted in formatteds:
        assert trumat.format(formatted) == formatted

def test_unformatted():
    for unformatted, formatted in unformatteds:
        assert trumat.format(unformatted) == formatted
