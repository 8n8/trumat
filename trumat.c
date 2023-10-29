#include "trumat.h"
#include <stdio.h>

void zero_memory(struct memory *m) {}

int write_chunk(uint8_t out[CODE_SIZE], int out_i, char* chunk) {
  int chunk_i = 0;
  for (; out_i + chunk_i < CODE_SIZE && chunk[chunk_i] != 0; ++chunk_i) {
    out[out_i + chunk_i] = chunk[chunk_i];
  }
  return out_i + chunk_i;
}

int format(uint8_t in[CODE_SIZE], uint8_t out[CODE_SIZE], struct memory *m) {
  int out_i = write_chunk(out, 0, "module X exposing (x)\n\n\nx =\n    0\n");

  out[out_i] = 0;

  return 0;
}
