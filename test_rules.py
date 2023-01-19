import rules


def test_rules():
    for case in cases:
        assert rules.format(case["input"]) == case["expected"]


cases = [
    {
        "input": """module X exposing (x)


x =
    0
""",
        "expected": """module X exposing (x)


x =
    0
""",
    },
    {
        "input": """module X exposing (x)


x = 
    0
""",
        "expected": """module X exposing (x)


x =
    0
""",
    },
    {
        "input": """module X exposing (x)


x =
    []
""",
        "expected": """module X exposing (x)


x =
    []
""",
    },
    {
        "input": """module X exposing (x)


x = 0
""",
        "expected": """module X exposing (x)


x =
    0
""",
    },
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
