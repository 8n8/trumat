# ded

A little helper for creating tooling for Elm code.

All it does is recursively read all the Elm files in the current directory and write them back again without changing them.

It's no use in itself, but is handy as a basis for creating tools for changing Elm code.

See the function `identity_pass` for an example transformation function.

## Build

This is tested in Ubuntu 22.04.1LTS, using GCC 11.2.0.

```
git clone git@github.com:8n8/ded.git
cd ded
gcc main.c -o ded -O2
```

This generates a binary called `ded`.
