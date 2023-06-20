#include <stdint.h>
#define BIG 1000000

struct Memory {
    uint8_t nesting[BIG];
};

int format(char in[BIG], char out[BIG], struct Memory*);
