#!/bin/python3

def format(code):
    equals_before = [False] * len(code)
    equals_before_state = False
    for i, c in enumerate(code):
        equals_before[i] = equals_before_state
        equals_before_state = c == "="

    equals_before1 = [False] * len(code)
    equals_before_state1 = False
    for i, c in enumerate(equals_before):
        equals_before1[i] = equals_before_state1
        equals_before_state1 = c

    newline_before = [False] * len(code)
    newline_before_state = False
    for i, c in enumerate(code):
        newline_before[i] = newline_before_state
        newline_before_state = c == "\n"


    return code

def elm_paths():
    elm_paths = []

    for root, _, file_names in os.walk("."):
        for file_name in file_names:
            if file_name[-4:] == ".elm":
                elm_paths.append(root + "/" + file_name)

    return elm_paths

def main():
    for elm_path in elm_paths():
        print(elm_path)
        with open(elm_path, "rb") as file_handle:
            code = file_handle.read()

        with open(elm_path, "wb") as file_handle:
            file_handle.write(format(code))


if __name__ == "__main__":
    main()
