#include "trumat.h"
#include <stdio.h>

void zero_memory(struct memory *m) {}

static int write_chunk(uint8_t out[CODE_SIZE], int out_i, char *chunk) {
  int chunk_i = 0;
  for (; out_i + chunk_i < CODE_SIZE && chunk[chunk_i] != 0; ++chunk_i) {
    out[out_i + chunk_i] = chunk[chunk_i];
  }
  return out_i + chunk_i;
}

static int parse_chunk(uint8_t in[CODE_SIZE], int in_i, char *chunk) {
  int chunk_i = 0;
  for (; in[chunk_i + in_i] == chunk[chunk_i] && chunk[chunk_i] != 0;
       ++chunk_i) {
  }

  if (chunk[chunk_i] == 0) {
    return in_i + chunk_i;
  }

  return -1;
}

int format(uint8_t in[CODE_SIZE], uint8_t out[CODE_SIZE], struct memory *m) {
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
