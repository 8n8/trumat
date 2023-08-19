#!/bin/python3

import os
import hashlib
import subprocess
import pathlib
import json

def init_db():
    return {
        # I chose 222 as an empty value so it's recognisable
        "original": [222] * 1000000,
        "size": 0,
    }


def is_multiline_module_exports(db):
    return False


def get_module_name(db):
    return { "start": 7, "size": 1 }


def zero_db(db):
    db["size"] = 0


def make_indexes(unformatted, db):
    for byte in unformatted:
        db["original"][db["size"]] = byte
        db["size"] += 1

def write_literal(out, literal):
    for char in literal:
        out["buffer"][out["size"]] = char
        out["size"] += 1


def write_chunk(out, chunk, db):
    for i in range(chunk["size"]):
        out['buffer'][out["size"]] = db["original"][chunk["start"] + i]
        out["size"] += 1


def get_export_name(export_id, db):
    if export_id == 0:
        return {"start": 19, "size": 1}


def is_export_expose_all(export_id, db):
    return False

def get_top_bind_name(bind_id, db):
    if bind_id == 0:
        return {"start": 24, "size": 1}

def get_top_bind_expression(top_bind_id, db):
    if top_bind_id == 0:
        return 0;

def get_verbatim(id, db):
    return { "start": db["size"] - 2, "size": 1 }

def write_verbatim(out, expression_id, db):
    verbatim = get_verbatim(expression_id, db)
    if verbatim is None:
        return

    write_chunk(out, verbatim, db)


def write_expression(out, expression_id, indent, db):
    write_verbatim(out, expression_id, db)


def write_out(out, db):
    write_literal(out, b"module ")
    write_chunk(out, get_module_name(db), db)
    write_literal(out, b" exposing")
    if is_multiline_module_exports(db):
        write_literal(out, b"\n    ( ")
    else:
        write_literal(out, b" (")

    separator = b", "
    if is_multiline_module_exports(db):
        separator = b"\n    , "

    for export_id in range(1000):
        export_name = get_export_name(export_id, db)
        if export_name is None:
            if is_multiline_module_exports(db):
                write_literal(out, b"\n    )")
            else:
                write_literal(out, b")")

            break

        if export_id != 0:
            write_literal(out, separator)

        write_chunk(out, export_name, db)
        if is_export_expose_all(export_id, db):
            write_literal(out, b"(..)")

    for top_bind_id in range(10000):
        name = get_top_bind_name(top_bind_id, db)
        if name is None:
            break

        write_literal(out, b"\n\n\n")
        write_chunk(out, name, db)
        write_literal(out, b" =\n    ")
        expression_id = get_top_bind_expression(top_bind_id, db)
        write_expression(out, expression_id, 4, db)
        write_literal(out, b"\n")


def format_help(unformatted, db):
    zero_db(db)
    make_indexes(unformatted, db)
    out = {
        "buffer": [111] * 1000000,
        "size": 0,
        }
    write_out(out, db)
    return out["buffer"][:out["size"]]


def elm_paths():
    elm_paths = []

    for root, _, file_names in os.walk("."):
        for file_name in file_names:
            if file_name[-4:] == ".elm":
                elm_paths.append(root + "/" + file_name)

    return elm_paths

def format(unformatted, db):
    result = format_help(list(unformatted), db)
    return bytes(result)

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
