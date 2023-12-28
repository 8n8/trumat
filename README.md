# trumat

A work in progress Elm formatter. Don't use it yet.

# Install

I think it won't work on Windows at the moment because I think listing directories in C is different in Windows. Ubuntu Linux definitely works and probably MacOS will work.

You need a C compiler, such as gcc, clang or zig. Then clone this repository and run, for example: `gcc main.c -o trumat` in the repository root. Then copy the `trumat` binary to somewhere on your path.

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

Developer dependencies are:

- `clang` because it has the best address and memory sanitisers
- `elm-format` for generating test cases
- `gdb` for debugging
- `diff` for showing test failures

I tried using Valgrind for detecting memory issues but it was too slow. It took several minutes to run the tests. However `clang` has some sanitiser options that do the job very quickly so I use those instead.

All the code is in the file `main.c`. The test files are in `input`. To create a new test, just add an Elm module to the `input` directory.

You can run the tests with `./test.sh` and run the debugger with `./debug.sh`.
