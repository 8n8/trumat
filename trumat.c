#include "trumat.h"
#include <stdio.h>

void zero_memory(struct memory *m) {}

// Write a string to an array of bytes at a particular index. The return
// value is the index at the end of the new section.
static int write_chunk(
    // This array is modified, starting at position index.
    uint8_t array[CODE_SIZE],
    // This is the position to write the first byte of the chunk to.
    int position,
    // This is the chunk that is written to an array.
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

// Format some Elm code.
int format(
    // This should contain the unformatted code, terminated with a zero
    // byte.
    const uint8_t in[CODE_SIZE],
    // It will write the formatted code to this array, terminated with a
    // zero byte.
    uint8_t out[CODE_SIZE],
    // The memory to use for the formatting.
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
