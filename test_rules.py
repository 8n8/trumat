import rules


def test_rules():
    for case in cases:
        got, err = rules.format(case["input"])
        assert err is None
        assert got == case["expected"]


cases = [
    #    {
    #        "input": """module X exposing (x)
    #
    #
    # x =
    #    0
    # """,
    #        "expected": """module X exposing (x)
    #
    #
    # x =
    #    0
    # """,
    #    },
    #    {
    #        "input": """module X exposing (x)
    #
    #
    # x =
    #    0
    # """,
    #        "expected": """module X exposing (x)
    #
    #
    # x =
    #    0
    # """,
    #    },
    #    {
    #        "input": """module X exposing (x)
    #
    #
    # x =
    #    []
    # """,
    #        "expected": """module X exposing (x)
    #
    #
    # x =
    #    []
    # """,
    #    },
    #    {
    #        "input": """module X exposing (x)
    #
    #
    # x = 0
    # """,
    #        "expected": """module X exposing (x)
    #
    #
    # x =
    #    0
    # """,
    #    },
    {
        "input": """module X exposing (x)


x =
    [ 0, 1
    ]
""",
        "expected": """module X exposing (x)


x =
    [ 0
    , 1
    ]
""",
    },
]
