# trumat

**work in progress - don't use yet**

A fast formatter for Elm, using the same rules as [elm-format](https://github.com/avh4/elm-format).

## Install

Run `stack install`. This will build the binary and copy it to `~/.local/bin`. You can add this to your `$PATH` by adding `export PATH=$HOME/.local/bin:$PATH` to your terminal configuration file, probably `~/.bashrc` or `~/.zshrc`. Restart the terminal and you should be able to run `trumat`.

## Run

At the moment, it doesn't accept any command-line options. IT ALSO OVER-WRITES FILES WITHOUT WARNING, SO BE CAREFUL. It recursively over-writes all the .elm files in the directory it is run in.

Be aware that it isn't finished yet, so doesn't do a very good job.

## Test

Run `stack test`.
