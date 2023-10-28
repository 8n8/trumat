#include "trumat.h"

void zero_memory(struct memory *m) {}

int format(uint8_t in[1000000], uint8_t out[1000000], struct memory *m) {
  for (int i = 0; i < 1000000; ++i) {
    out[i] = in[i];
  }
  return 0;
}
