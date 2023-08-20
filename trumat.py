#!/bin/python3

def parse_column_numbers(code):
    column_numbers = [0] * len(code)
    column_number = 0
    for i, c in enumerate(code):
        column_numbers[i] = column_number

        if c == '\n':
            column_number = 0
        else:
            column_number += 1


    return column_numbers

def format(code):
    columns = parse_column_numbers(code)

    i = 0
    while code[i] == '\n' or code[i] == ' ':
        i += 1

    if columns[i] != 0:
        raise ValueError("should be first column")

    return code

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
