# trumat

**work in progress - it's safe to use but doesn't do much yet**

A code formatter for Elm. The aim is to replicate the rules used in [elm-format](https://github.com/avh4/elm-format).

Currently it doesn't do much but it shouldn't do any damage.

## Install

You need to build it from source.

It's currently only tested on Ubuntu Linux, but should work on MacOS. I think it won't work on Windows yet.

You need to have clang (C compiler) installed.

Then:

- clone this repository
- in the root of this repository run `./build.sh`
- you should now have a binary called `trumat` which can be run where it is with `./trumat`, or you can copy it to somewhere on your PATH, such as `/usr/local/bin`

(The PATH is an environment variable that specifies the directories that contain the programs you can run directly by name from your terminal. To see your current PATH run `echo $PATH`.)

## Usage

IT'S A GOOD IDEA TO BACK UP YOUR CODE FIRST BECAUSE IT WILL OVERWRITE FILES.

Run `trumat --overwrite` to recursively format and overwrite all the Elm files in the current directory.

Note that it will currently fail to do anything and print an error message for most files as there is a lot of Elm that is not supported yet. You can suppress these error messages by redirecting them to `/dev/null` as follows: `trumat --overwrite 2> /dev/null`.

## Development

To test the code for development work, run `./test.sh`.

The test data is kept in `formatted`, `expected`, `input` and `dont_change`.

The `dont_change` directory contains a lot of Elm files that are already formatted with `elm_format`. The test runner passes all these through `trumat` and checks it doesn't change any of them.

The `input` directory contains unformatted Elm code. The `expected` directory contains the same paths as `input`, but all the files are formatted. The test runner passes each file in `input` through `trumat` and checks the result against the corresponding file in `expected`.

The `formatted` directory contains formatted Elm code. The difference from `dont_change` is that these tests fail if the formatter fails. It's a stronger test than the ones in `dont_change`. `dont_change` is used for real Elm code downloaded from GitHub, and `formatted` is used for minimal test files made for testing.

The test script runs the tests through clang's address and memory sanitizers, and also checks that the main runner builds. Clang's sanitizers are much faster than Valgrind and mostly cover the same things.

If you need to run a debugger you can build the tests and run them with `./debug.sh`.
