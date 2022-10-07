# ded

A work in progress towards making better Elm tooling.

All it does at the moment is to remove trailing spaces from all the lines in all the Elm files in the current directory and below.

Be aware that it overwrites all the Elm files.

It works OK in Ubuntu Linux. It currently assumes that lines end with `\n` and that paths are separated with `/`.

## Build

This is tested in Ubuntu 22.04.1LTS, using GCC 11.2.0.

```
git clone git@github.com:8n8/ded.git
cd ded
gcc main.c -o ded -O2
```

This generates a binary called `ded`.

## Run

Run the binary generated above in the directory that you want to analyse. There are no arguments.
