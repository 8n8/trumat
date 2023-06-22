#include <stdint.h>
#define BIG 1000000

struct Memory {
        uint16_t row_numbers[BIG];
        uint16_t column_numbers[BIG];
        uint8_t tokens[BIG];
        uint32_t token_start[BIG];
        uint8_t token_size[BIG];
};

int format(char in[BIG], char out[BIG], struct Memory* memory);
