#!/bin/python3

def parse_integer(code, i):
    start = i
    while code[i] != '\n' and code[i] != ' ':
        i += 1
    return code[start:i]

def format(code):
    preamble = """module X exposing (x)


x =
"""
    i = len(preamble)
    while code[i] == " ":
        i += 1

    integer = parse_integer(code, i)
    return f"{preamble}    {integer}\n"

def elm_paths():
    elm_paths = []

    for root, _, file_names in os.walk("."):
        for file_name in file_names:
            if file_name[-4:] == ".elm":
                elm_paths.append(root + "/" + file_name)

    return elm_paths

def main():
    db = init_db()
    for elm_path in elm_paths():
        print(elm_path)
        with open(elm_path, "rb") as file_handle:
            code = file_handle.read()

        with open(elm_path, "wb") as file_handle:
            file_handle.write(format(code, db))


if __name__ == "__main__":
    main()
