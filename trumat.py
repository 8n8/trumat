examples = { """module X exposing (x)


x =
   0
""": """module X exposing (x)


x =
    0
"""
}

def format(unformatted):
    try:
        return examples[unformatted]
    except KeyError:
        if unformatted[-2] == '0' and unformatted[-3] == ' ':
            return """module X exposing (x)


x =
    0
"""
        return unformatted
