# trumat

A work in progress Elm formatter. Don't use it yet.

# Install

It definitely runs in Ubuntu Linux and probably in MacOS.

Steps:

1. Install the Haskell tool stack: https://docs.haskellstack.org/en/stable/install_and_upgrade/
2. run `stack install` in the root of this repository

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

Install the Haskell tool stack: https://docs.haskellstack.org/en/stable/install_and_upgrade/

The test files are in `input`. To create a new test just add an Elm module to the `input` directory. The test runner will make two copies, format one with `elm-format` and the other with `trumat`, and compare the results.

Then run the tests with `./test.sh`.
