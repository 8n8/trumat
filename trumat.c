#include "trumat.h"


/*

The nesting levels are like this:

module X exposing (x)\n\n\nx =\n    [0]\n
00000011111111111112100000001111111112100

The nesting types are like this:

module X exposing (x)\n\n\nx =\n    [0]\n
00000011111111111112100000003333333334300

where

    0 == top level
    1 == module declaration
    2 == module export list
    3 == top level bind
    4 == int literal

Say I want to format level 1, I can black out
levels 0 and 2:

###### X exposing (#)####### =\n    [#]##    masked
00000011111111111112100000003333333334300    types

How to calculate the nesting levels though?

And how to calculate the types?

I don't know.

*/
int format(char in[BIG], char out[BIG], struct Memory* memory) {
}
