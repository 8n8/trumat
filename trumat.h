#include <stdint.h>
#include <stdio.h>

struct Memory {
  int raw_length;
  uint8_t raw[1000000];
  uint8_t chars[1000000];

  // These are used during tokenisation.
  uint8_t tokeniser_state[1000000];
  uint8_t is_no_tokens[125000];
  uint8_t is_simple_token[125000];
  uint8_t is_compound_token[125000];
  uint8_t compound_token[1000000];

  // These are the result of tokenisation.
  int tokens_length;
  uint8_t token_type[500000];
  uint32_t token_start[500000];
  uint16_t token_length[500000];
};

void zero_memory(struct Memory *);

int format(FILE *, FILE *, struct Memory *);
