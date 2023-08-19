import trumat

formatteds = [
    b"""module X exposing (x)


x =
    0
"""
]

unformatteds = [
    ( b"""module X exposing (x)


x =
   0
""",
    b"""module X exposing (x)


x =
    0
""")
]


def test_formatted():
    db = trumat.init_db()
    for formatted in formatteds:
        assert trumat.format(formatted, db) == formatted

def test_unformatted():
    db = trumat.init_db()
    for unformatted, formatted in unformatteds:
        assert trumat.format(unformatted, db) == formatted
