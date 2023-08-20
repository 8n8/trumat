import trumat

def test_hello_world_formatted():
    input = """module X exposing (x)


x =
    0
"""
    assert trumat.format(input) == input

def test_spaces_before_module_keyword():
    input = """ module X exposing (x)


x =
    0
"""
    try:
        trumat.format(input)
        assert False
    except ValueError:
        assert True
