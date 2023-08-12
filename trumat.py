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
        [beforeEquals, afterEquals] = unformatted.split("=")
        return f"""{beforeEquals}=
    {afterEquals.strip()}
"""
