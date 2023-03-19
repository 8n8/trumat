#include "main.h"
#include <stdio.h>

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
            continue;
        }

        for (int x = 0; x < mem->insert_newline[i]; ++x) {
            out[j] = '\n';
            ++j;
            continue;
        }

        out[j] = in[i];
        ++j;
    }

    return j;
}

void calculate_followed_by_newline(
    uint8_t in[MAX_BUF],
    int size,
    uint8_t followed_by_newline[MAX_BUF]) {

    for (int i = 0; i < size - 1; ++i) {
        if (in[i+1] == '\n') {
            followed_by_newline[i] = 1;
        }
    }

    followed_by_newline[size - 1] = 0;
}

void calculate_contexts(uint8_t in[MAX_BUF], int size, struct Mem* mem) {
    calculate_followed_by_newline(in, size, mem->followed_by_newline);
}

void calculate_delete_space(
    uint8_t in[MAX_BUF],
    int size,
    struct Mem* mem) {
    
    for (int i = 0; i < size; ++i) {
        mem->delete_space[i] = in[i] == ' ' && mem->followed_by_newline[i];
    }
}

void calculate_edits(uint8_t in[MAX_BUF], int size, struct Mem* mem) {
    calculate_delete_space(in, size, mem);
}

int format(
    uint8_t in[MAX_BUF],
    int size,
    uint8_t out[MAX_BUF],
    struct Mem* mem) {

    calculate_contexts(in, size, mem);

    calculate_edits(in, size, mem);

    return apply_edits(in, size, out, mem);
}
