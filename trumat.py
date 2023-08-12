def format(unformatted):
    return f"""{unformatted.split("=")[0]}=
    {unformatted.split("=")[1].strip()}
"""
