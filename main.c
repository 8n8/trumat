#include "main.h"

int apply_edits(
    uint8_t in[MAX_BUF],
    int size,
    uint8_t out[MAX_BUF],
    struct Mem* mem) {

    int j = 0;

    for (int i = 0; i < size; ++i) {
        if (mem->delete_space[i]) {
            continue;
        }

        if (mem->delete_newline[i]) {
            continue;
        }

        for (int x = 0; x < mem->insert_space[i]; ++x) {
            out[j] = ' ';
            ++j;
        }

        for (int x = 0; x < mem->insert_newline[i]; ++x) {
            out[j] = '\n';
            ++j;
        }
    }

    return j;
}

int format(
    uint8_t in[MAX_BUF],
    int size,
    uint8_t out[MAX_BUF],
    struct Mem* mem) {

    for (int i = 0; i < size; ++i) {
    }

    return apply_edits(in, size, out, mem);
}
