def format(unformatted):
    return f"""{unformatted.split("=")[0].strip()} =
    {unformatted.split("=")[1].strip()}
"""
