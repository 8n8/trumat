# trumat

A work in progress Elm formatter. Don't use it yet.

# Install

You need a C compiler, such as gcc, clang or zig. Then clone this repository and run, for example: `gcc main.c -o trumat`. Then copy the `trumat` binary to somewhere on your path.

# Usage

It requires two arguments in this order:

1. `--overwrite` to confirm you understand it will recursively overwrite all the Elm files in the path
2. The path to use. This can be an Elm file or a directory containing Elm files

For example:

```
$ trumat --overwrite A.elm
```

or

```
$ trumat --overwrite src
```

or

```
$ trumat --overwrite .
```

# Development

All the code is in the file `main.c`. The tests files are in `formatted` and `unformatted`.

You can run the tests with `./test.sh` and run the debugger with `./debug.sh`.
