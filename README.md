# ded

A work in progress towards making a faster Elm formatter. It's not ready yet.

The goal is to behave the same as `elm-format . --yes` but faster. So far, it:

- removes trailing spaces
- adds a newline after top-level binds
- indents the body of top-level binds by 4 spaces

Be aware that it overwrites all the Elm files.

It works OK in Ubuntu Linux. It currently assumes that lines end with `\n` and that paths are separated with `/`.

## Build

You need a C compiler.

This is tested in Ubuntu 22.04.1LTS, using GCC 11.2.0.

```
git clone git@github.com:8n8/ded.git
cd ded
gcc main.c -o ded -O2
```

This generates a binary called `ded`.

## Run

Run the binary generated above in the directory that you want to analyse. There are no arguments.
