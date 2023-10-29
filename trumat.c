#include "trumat.h"

void zero_memory(struct memory *m) {}

int format(uint8_t in[CODE_SIZE], uint8_t out[CODE_SIZE], struct memory *m) {
  int i = 0;
  for (; i < CODE_SIZE && in[i] != 0; ++i) {
    out[i] = in[i];
  }

  out[i] = 0;

  return 0;
}
