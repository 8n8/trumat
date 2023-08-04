#include <stdint.h>
#include <stdio.h>

struct Memory {
  int raw_length;
  uint8_t raw[1000000];
  uint8_t chars[1000000];

  uint16_t row[1000000];
  uint16_t column[1000000];

  uint8_t tokens[1000000];

  uint16_t min_column[1000000];

  uint8_t nesting[1000000];
};

void zero_memory(struct Memory *);

int format(FILE *, FILE *, struct Memory *);
