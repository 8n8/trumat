#!/bin/python3

import subprocess
import pathlib
import json

def elm_paths():
    elm_paths = []

    for root, _, file_names in os.walk("."):
        for file_name in file_names:
            if file_name[-4:] == ".elm":
                elm_paths.append(root + "/" + file_name)

    return elm_paths

def format(unformatted):
    return unformatted

def main():
    db = init_db()
    for elm_path in elm_paths():
        print(elm_path)
        with open(elm_path, "rb") as file_handle:
            unformatted = file_handle.read()

        with open(elm_path, "wb") as file_handle:
            file_handle.write(format(unformatted, db))


if __name__ == "__main__":
    main()
