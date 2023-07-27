#include <stdint.h>
#include <stdio.h>

struct u8x1m {
  int length;
  uint8_t items[1000000];
};

struct u8x500k {
  int length;
  uint8_t items[500000];
};

struct u32x500k {
  int length;
  uint32_t items[500000];
};

struct u16x500k {
  int length;
  uint16_t items[500000];
};

struct Memory {
  struct u8x1m raw;
  struct u8x1m chars;

  // These are used during tokenisation.
  struct u8x1m tokeniser_state;
  struct u8x1m num_tokens;
  struct u8x1m first_token;
  struct u8x1m second_token;

  // These are the result of tokenisation.
  struct u8x500k token_type;
  struct u32x500k token_start;
  struct u16x500k token_size;
};

void zero_memory(struct Memory *);

int format(FILE *, FILE *, struct Memory *);
