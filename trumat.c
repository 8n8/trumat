#include "trumat.h"

enum elm_char {
    ch_0,
    ch_equals,
    ch_newline,
    ch_close_parens,
    ch_open_parens,
    ch_g,
    ch_n,
    ch_i,
    ch_s,
    ch_o,
    ch_p,
    ch_x,
    ch_X,
    ch_space,
    ch_e,
    ch_l,
    ch_u,
    ch_d,
    ch_m,
};

void zero_memory(struct Memory* memory) {
}

static int read_raw(FILE* input_file, struct u8x1m* buffer) {
    buffer->length = fread(buffer->items, 1, 1000000, input_file);

    int result = ferror(input_file);
    if (result != 0) {
        return result;
    }

    result = feof(input_file);
    if (result == 0) {
        return -1;
    }

    return 0;
}

static int get_u8x1m(struct u8x1m* raw, int index) {
    if (index >= raw->length || index < 0) {
        return -1;
    }

    return raw->items[index];
}

static int parse_char(uint8_t raw) {
    switch (raw) {
    case 'm':
        return ch_m;
    case 'd':
        return ch_d;
    case 'u':
        return ch_u;
    case 'l':
        return ch_l;
    case 'e':
        return ch_e;
    case ' ':
        return ch_space;
    case 'X':
        return ch_X;
    case 'x':
        return ch_x;
    case 'p':
        return ch_p;
    case 'o':
        return ch_o;
    case 's':
        return ch_s;
    case 'i':
        return ch_i;
    case 'n':
        return ch_n;
    case 'g':
        return ch_g;
    case '(':
        return ch_open_parens;
    case ')':
        return ch_close_parens;
    case '\n':
        return ch_newline;
    case '=':
        return ch_equals;
    case '0':
        return ch_0;
    }
    return -1;
}

static int parse_chars(struct u8x1m* raw, struct u8x1m* chars) {
    for (int i = 0; ; ++i) {
        int result = get_u8x1m(raw, i);
        if (result < 0) {
            return 0;
        }

        result = parse_char(result);
        if (result < 0) {
            return result;
        }
    }
}

int format(FILE* input_file, FILE* output_file, struct Memory* memory) {
    int result = read_raw(input_file, &memory->raw);
    if (result != 0) {
        return result;
    }

    return parse_chars(&memory->raw, &memory->chars);
}
