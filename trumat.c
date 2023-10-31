#include "trumat.h"
#include <stdio.h>

void zero_memory(struct memory *m) {}

// Write a string to an array of bytes at a particular index. The return
// value is the first index after the end of the new section.
//
// If there isn't enough room in the array then it writes as much as it can.
static int write_chunk(
    // The string is written into this array, starting at the given
    // position.
    uint8_t array[CODE_SIZE],
    // This is the position to write the first byte of the chunk to.
    int position,
    // This is the string that is written to the array.
    const char *chunk) {

  int chunk_i = 0;
  for (; position + chunk_i < CODE_SIZE && chunk[chunk_i] != 0; ++chunk_i) {
    array[position + chunk_i] = chunk[chunk_i];
  }
  return position + chunk_i;
}

// Checks that an array contains a string, starting at an index.
//
// It returns a negative number if the string isn't in the array, starting
// at the provided position.
//
// It returns the position of the end of the string in the array if it is
// there.
static int parse_chunk(
    // The array to search
    const uint8_t array[CODE_SIZE],
    // The position in the array to start the search.
    int position,
    // The chunk to search for.
    const char *chunk) {

  int chunk_i = 0;
  for (; array[chunk_i + position] == chunk[chunk_i] && chunk[chunk_i] != 0;
       ++chunk_i) {
  }

  if (chunk[chunk_i] == 0) {
    return position + chunk_i;
  }

  return -1;
}

int format(const uint8_t in[CODE_SIZE], uint8_t out[CODE_SIZE],
           struct memory *m) {

  int in_i = 0;
  int result = parse_chunk(in, in_i, "module X exposing (x)\n\n\nx =\n");
  if (result < 0) {
    return result;
  }
  in_i = result;

  for (; in[in_i] == ' '; ++in_i) {
  }

  result = parse_chunk(in, in_i, "0\n\0");
  if (result < 0) {
    return result;
  }

  write_chunk(out, 0, "module X exposing (x)\n\n\nx =\n    0\n\0");

  return 0;
}
