#include "trumat.h"

#define MAX_NESTING 20

int calculate_row_numbers(char in[BIG], uint16_t row_numbers[BIG]) {
    int row_number = 0;
    for (int i = 0; i < BIG; ++i) {
        row_numbers[i] = row_number;
        if (in[i] == 0) {
            return 0;
        }
        if (in[i] == '\n') {
            ++row_number;
        }
        if (row_number == 1 << 16) {
            return -1;
        }
    }

    return 0;
}

int calculate_column_numbers(char in[BIG], uint16_t column_numbers[BIG]) {
    int column_number = 0;
    for (int i = 0; i < BIG; ++i) {
        column_numbers[i] = column_number;
        if (in[i] == 0) {
            return 0;
        }
        if (in[i] == '\n') {
            column_number = 0;
        } else {
            ++column_number;
        }
        if (column_number == 1 << 16) {
            return -1;
        }
    }

    return 0;
}

int format(char in[BIG], char out[BIG], struct Memory* memory) {
    int result = calculate_row_numbers(in, memory->row_numbers);
    if (result != 0) {
        return result;
    }

    result = calculate_column_numbers(in, memory->column_numbers);
    if (result != 0) {
        return result;
    }

    return 0;
}
