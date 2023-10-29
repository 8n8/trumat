#include <stdint.h>
// I found an elm file in elm-review that was approx 700 KB.
#define CODE_SIZE 2000*1000
struct memory {};

void zero_memory(struct memory *);

int format(uint8_t in[CODE_SIZE], uint8_t out[CODE_SIZE], struct memory *m);
