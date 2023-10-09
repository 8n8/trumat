# trumat

**work in progress - don't use yet**

A fast formatter for Elm, using the same rules as [elm-format](https://github.com/avh4/elm-format).

## Install

First install the Haskell Tool Stack: https://docs.haskellstack.org/en/stable/#how-to-install-stack

Run `stack install` in the root of this repository. This will take several minutes and will download the compiler and dependencies, build the binary and copy it to `~/.local/bin`. You can add this to your `$PATH` by adding `export PATH=$HOME/.local/bin:$PATH` to your terminal configuration file, probably `~/.bashrc` or `~/.zshrc`. Restart the terminal.

## (Don't) Run

At the moment, it doesn't accept any command-line options. IT ALSO OVER-WRITES FILES WITHOUT WARNING, SO BE CAREFUL. It recursively over-writes all the .elm files in the directory it is run in.

Run `trumat` in your terminal in the directory containing the .elm files.

Be aware that it isn't finished yet, so doesn't do a very good job.

## Test

Run `stack test`.
