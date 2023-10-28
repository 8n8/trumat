#include <stdint.h>

struct memory {};

void zero_memory(struct memory*);

int format(uint8_t in[1000000], uint8_t out[1000000], struct memory *m);
