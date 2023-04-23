import trumat

def test_hello_world_formatted():
    got = trumat.format("""module X exposing (x)


x =
    0
""") 
    expected = """module X exposing (x)


x =
    0
"""
    assert got == expected

def test_hello_world_unformatted():
    got = trumat.format("""module X exposing (x)


x =
   0
""") 
    expected = """module X exposing (x)


x =
    0
"""
    assert got == expected

def test_empty_list_formatted():
    got = trumat.format("""module X exposing (x)


x =
    []
""") 
    expected = """module X exposing (x)


x =
    []
"""
    assert got == expected
