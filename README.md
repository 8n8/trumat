# trumat

**work in progress - it's safe to use but doesn't do much yet**

A code formatter for Elm. The aim is to replicate the rules used in [elm-format](https://github.com/avh4/elm-format).

Currently it doesn't do much, but is well tested and shouldn't do any damage.

## Install

You need to build it from source.

It's currently only tested on Ubuntu Linux, but should work on MacOS. I think it won't work on Windows.

You need a C compiler, such as gcc or clang. You may have one already. Run `cc --version` to find out.

Then:

- clone this repository
- in the root of this repository run `cc -c -O2 trumat.c && cc -O2 trumat.o main.c --output=trumat`
- you should now have a binary called `trumat` which can be run where it is with `./trumat`, or you can copy it to somewhere on your PATH, such as `/usr/local/bin`

(The PATH is an environment variable that specifies the directories that contain the programs you can run directly by name from your terminal. To see your current PATH run `echo $PATH`.)

## Usage

IT'S A GOOD IDEA TO BACK UP YOUR CODE FIRST BECAUSE IT WILL OVERWRITE FILES.

Run `trumat --overwrite` to recursively format and overwrite all the Elm files in the current directory.

## Test

To test the code for development work, run `cc -c -Wall -Werror -O2 -std=c99 trumat.c && cc -Wall -O2 -Werror -std=c99 trumat.o test.c && ./a.out`.

The test data is kept in `test_formatted`, `test_expected` and `test_input`.

The `test_formatted` directory contains a lot of Elm files that are already formatted with `elm_format`. The test runner passes all these through `trumat` and checks it doesn't change any of them.

The `test_input` directory contains unformatted Elm code. The `test_expected` directory contains the same paths as `test_input`, but all the files are formatted. The test runner passes each file in `test_input` through `trumat` and checks the result against the corresponding file in `test_expected`.
