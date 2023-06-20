#include "trumat.h"
#include <stdio.h>

int calculate_top_level_whitespace_nesting(
    char in[BIG],
    uint8_t nesting[BIG],
    int i) {

    for (; (in[i] == '\n' || in[i] == ' ') && i < BIG; ++i) {
        nesting[i] = 0;
    }

    return i;
}

int calculate_module_declaration_nesting(
    char in[BIG],
    uint8_t nesting[BIG],
    int i) {

    int j = parse_keyword(in, "module", i);
    if (j < 0) {
        return j;
    }

    for (; i < j; ++i) {
        
    }
}

int calculate_top_level_item_nesting(
    char in[BIG],
    uint8_t nesting[BIG],
    int i) {

    return calculate_module_declaration_nesting(in, nesting, i);
}

int calculate_top_level_nesting(char in[BIG], uint8_t nesting[BIG], int i) {
    int result = calculate_top_level_whitespace_nesting(in, nesting, i);
    if (i < 0 || in[i] == 0) {
        return i;
    }

    return calculate_top_level_item_nesting(in, nesting, i);
}

int calculate_nesting(char in[BIG], uint8_t nesting[BIG]) {
    for (int i = 0; i < BIG && in[i] != 0; ++i) {
        i = calculate_top_level_nesting(in, nesting, i);
    }

    return 0;
}

int format(char in[BIG], char out[BIG], struct Memory* memory) {
    return calculate_nesting(in, memory->nesting);
}
