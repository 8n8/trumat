import rules


def t(input, expected):
    got, err = rules.format(input)
    assert err is None
    assert got == expected


def test_hello_world_formatted():
    t(
        """module X exposing (x)


x =
    0
""",
        """module X exposing (x)


x =
    0
""",
    )


def test_empty_list_formatted():
    t(
        """module X exposing (x)


x =
    []
""",
        """module X exposing (x)


x =
    []
""",
    )


def test_newline_after_top_level_bind():
    t(
        """module X exposing (x)


x = 0
""",
        """module X exposing (x)


x =
    0
""",
    )


def test_flat_newline_list():
    t(
        """module X exposing (x)


x =
    [ 0, 1
    ]
""",
        """module X exposing (x)


x =
    [ 0
    , 1
    ]
""",
    )


def test_nested_flat_list():
    t(
        """module X exposing (x)


x =
    [ 0, [2] ]
""",
        """module X exposing (x)


x =
    [ 0, [ 2 ] ]
""",
    )


def test_nested_newline_lists():
    t(
        """module X exposing (x)


x =
    [a, [ 1
    , 2]
    ]
""",
        """module X exposing (x)


x =
    [ a
    , [ 1
      , 2
      ]
    ]
""",
    )


def test_many_nested_list():
    t(
        """module X exposing (x)


x=[[[[[
]]]]]
""",
        """module X exposing (x)


x =
    [ [ [ [ []
          ]
        ]
      ]
    ]
""",
    )


def test_if_then_else_single_line_formatted():
    t(
        """module X exposing (x)


x =
    if a then b else c
""",
        """module X exposing (x)


x =
    if a then
        b

    else
        c
""",
    )


def test_if_then_else_multi_line_formatted():
    t(
        """module X exposing (x)


x =
    if a then
        b

    else
        c
""",
        """module X exposing (x)


x =
    if a then
        b

    else
        c
""",
    )
