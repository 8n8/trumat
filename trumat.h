#include <stdint.h>
#define CODE_SIZE 1000*1000
struct memory {};

void zero_memory(struct memory *);

int format(uint8_t in[CODE_SIZE], uint8_t out[CODE_SIZE], struct memory *m);
