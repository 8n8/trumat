#define BIG 1000000

struct Memory {
        uint16_t row_numbers[BIG];
        uint16_t column_numbers[BIG];
        uint8_t token
}

int format(char in[BIG], char out[BIG], struct Memory* memory);
