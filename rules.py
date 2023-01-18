rules = []


def format(code):
    for rule in rules:
        code = rule(code)

    return code
