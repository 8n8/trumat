#include <stdint.h>
#include <stdio.h>

struct Memory {
  int raw_length;
  uint8_t raw[1000000];
  uint8_t chars[1000000];

  // These are the result of tokenisation.
  uint8_t tokens[1000000];
};

void zero_memory(struct Memory *);

int format(FILE *, FILE *, struct Memory *);
