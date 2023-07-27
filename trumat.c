#include "trumat.h"

enum elm_char {
    char_0,
    char_equals,
    char_newline,
    char_close_parens,
    char_open_parens,
    char_g,
    char_n,
    char_i,
    char_s,
    char_o,
    char_p,
    char_x,
    char_X,
    char_space,
    char_e,
    char_l,
    char_u,
    char_d,
    char_m,
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

static int u8x1m_append(enum elm_char ch, struct u8x1m* chars) {
    if (chars->length == 1000000) {
        return -1;
    }

    chars->items[chars->length] = ch;
    ++chars->length;

    return 0;
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

        result = u8x1m_append(result, chars);
        if (result != 0) {
            return result;
        }
    }
}

enum tokeniser {
    tokeniser_start,
    tokeniser_m,
    tokeniser_l,
    tokeniser_e,
    tokeniser_upper_name,
    tokeniser_p,
    tokeniser_o,
    tokeniser_i,
    tokeniser_lower_name,
    tokeniser_0,
};

// reserved keywords in Elm: as case else exposing if import in let module
// of port then type where
static int tokeniser_state_machine(enum tokeniser state, enum elm_char ch) {
    switch (state) {
    case tokeniser_upper_name:
        switch (ch) {
        case char_equals:
            return tokeniser_start;
        case char_0:
            return tokeniser_upper_name;
        case char_newline:
            return tokeniser_start;
        case char_close_parens:
            return tokeniser_start;
        case char_open_parens:
            return tokeniser_start;
        case char_g:
            return tokeniser_upper_name;
        case char_n:
            return tokeniser_upper_name;
        case char_i:
            return tokeniser_upper_name;
        case char_s:
            return tokeniser_upper_name;
        case char_o:
            return tokeniser_upper_name;
        case char_p:
            return tokeniser_upper_name;
        case char_x:
            return tokeniser_upper_name;
        case char_X:
            return tokeniser_upper_name;
        case char_space:
            return tokeniser_start;
        case char_e:
            return tokeniser_upper_name;
        case char_l:
            return tokeniser_el;
        case char_m:
            return tokeniser_upper_name;
        case char_d:
            return tokeniser_upper_name;
        case char_u:
            return tokeniser_upper_name;
        };
    case tokeniser_e:
        switch (ch) {
        case char_equals:
            return tokeniser_start;
        case char_0:
            return tokeniser_lower_name;
        case char_newline:
            return tokeniser_start;
        case char_close_parens:
            return tokeniser_start;
        case char_open_parens:
            return tokeniser_start;
        case char_g:
            return tokeniser_lower_name;
        case char_n:
            return tokeniser_lower_name;
        case char_i:
            return tokeniser_lower_name;
        case char_s:
            return tokeniser_lower_name;
        case char_o:
            return tokeniser_lower_name;
        case char_p:
            return tokeniser_lower_name;
        case char_x:
            return tokeniser_ex;
        case char_X:
            return tokeniser_lower_name;
        case char_space:
            return tokeniser_start;
        case char_e:
            return tokeniser_le;
        case char_l:
            return tokeniser_el;
        case char_m:
            return tokeniser_lower_name;
        case char_d:
            return tokeniser_lower_name;
        case char_u:
            return tokeniser_lower_name;
        };
    case tokeniser_l:
        switch (ch) {
        case char_equals:
            return tokeniser_start;
        case char_0:
            return tokeniser_lower_name;
        case char_newline:
            return tokeniser_start;
        case char_close_parens:
            return tokeniser_start;
        case char_open_parens:
            return tokeniser_start;
        case char_g:
            return tokeniser_lower_name;
        case char_n:
            return tokeniser_lower_name;
        case char_i:
            return tokeniser_lower_name;
        case char_s:
            return tokeniser_lower_name;
        case char_o:
            return tokeniser_lower_name;
        case char_p:
            return tokeniser_lower_name;
        case char_x:
            return tokeniser_lower_name;
        case char_X:
            return tokeniser_lower_name;
        case char_space:
            return tokeniser_start;
        case char_e:
            return tokeniser_le;
        case char_l:
            return tokeniser_lower_name;
        case char_m:
            return tokeniser_lower_name;
        case char_d:
            return tokeniser_lower_name;
        case char_u:
            return tokeniser_lower_name;
        };
    case tokeniser_m:
        switch (ch) {
        case char_equals:
            return tokeniser_start;
        case char_0:
            return tokeniser_lower_name;
        case char_newline:
            return tokeniser_start;
        case char_close_parens:
            return tokeniser_start;
        case char_open_parens:
            return tokeniser_start;
        case char_g:
            return tokeniser_lower_name;
        case char_n:
            return tokeniser_lower_name;
        case char_i:
            return tokeniser_lower_name;
        case char_s:
            return tokeniser_lower_name;
        case char_o:
            return tokeniser_mo;
        case char_p:
            return tokeniser_lower_name;
        case char_x:
            return tokeniser_lower_name;
        case char_X:
            return tokeniser_lower_name;
        case char_space:
            return tokeniser_start;
        case char_e:
            return tokeniser_lower_name;
        case char_l:
            return tokeniser_lower_name;
        case char_m:
            return tokeniser_lower_name;
        case char_d:
            return tokeniser_lower_name;
        case char_u:
            return tokeniser_lower_name;
        };
    case tokeniser_p:
        switch (ch) {
        case char_equals:
            return tokeniser_start;
        case char_0:
            return tokeniser_lower_name;
        case char_newline:
            return tokeniser_start;
        case char_close_parens:
            return tokeniser_start;
        case char_open_parens:
            return tokeniser_start;
        case char_g:
            return tokeniser_lower_name;
        case char_n:
            return tokeniser_lower_name;
        case char_i:
            return tokeniser_lower_name;
        case char_s:
            return tokeniser_lower_name;
        case char_o:
            return tokeniser_po;
        case char_p:
            return tokeniser_lower_name;
        case char_x:
            return tokeniser_lower_name;
        case char_X:
            return tokeniser_lower_name;
        case char_space:
            return tokeniser_start;
        case char_e:
            return tokeniser_lower_name;
        case char_l:
            return tokeniser_lower_name;
        case char_m:
            return tokeniser_lower_name;
        case char_d:
            return tokeniser_lower_name;
        case char_u:
            return tokeniser_lower_name;
        };
    case tokeniser_o:
        switch (ch) {
        case char_equals:
            return tokeniser_start;
        case char_0:
            return tokeniser_lower_name;
        case char_newline:
            return tokeniser_start;
        case char_close_parens:
            return tokeniser_start;
        case char_open_parens:
            return tokeniser_start;
        case char_g:
            return tokeniser_lower_name;
        case char_n:
            return tokeniser_lower_name;
        case char_i:
            return tokeniser_lower_name;
        case char_s:
            return tokeniser_lower_name;
        case char_o:
            return tokeniser_lower_name;
        case char_p:
            return tokeniser_lower_name;
        case char_x:
            return tokeniser_lower_name;
        case char_X:
            return tokeniser_lower_name;
        case char_space:
            return tokeniser_start;
        case char_e:
            return tokeniser_lower_name;
        case char_l:
            return tokeniser_lower_name;
        case char_m:
            return tokeniser_lower_name;
        case char_d:
            return tokeniser_lower_name;
        case char_u:
            return tokeniser_lower_name;
        };
    case tokeniser_i:
        switch (ch) {
        case char_equals:
            return tokeniser_start;
        case char_0:
            return tokeniser_lower_name;
        case char_newline:
            return tokeniser_start;
        case char_close_parens:
            return tokeniser_start;
        case char_open_parens:
            return tokeniser_start;
        case char_g:
            return tokeniser_lower_name;
        case char_n:
            return tokeniser_in;
        case char_i:
            return tokeniser_lower_name;
        case char_s:
            return tokeniser_lower_name;
        case char_o:
            return tokeniser_lower_name;
        case char_p:
            return tokeniser_lower_name;
        case char_x:
            return tokeniser_lower_name;
        case char_X:
            return tokeniser_lower_name;
        case char_space:
            return tokeniser_start;
        case char_e:
            return tokeniser_lower_name;
        case char_l:
            return tokeniser_lower_name;
        case char_m:
            return tokeniser_im;
        case char_d:
            return tokeniser_lower_name;
        case char_u:
            return tokeniser_lower_name;
        };
    case tokeniser_lower_name:
        switch (ch) {
        case char_equals:
            return tokeniser_start;
        case char_0:
            return tokeniser_lower_name;
        case char_newline:
            return tokeniser_start;
        case char_close_parens:
            return tokeniser_start;
        case char_open_parens:
            return tokeniser_start;
        case char_g:
            return tokeniser_lower_name;
        case char_n:
            return tokeniser_lower_name;
        case char_i:
            return tokeniser_lower_name;
        case char_s:
            return tokeniser_lower_name;
        case char_o:
            return tokeniser_lower_name;
        case char_p:
            return tokeniser_lower_name;
        case char_x:
            return tokeniser_lower_name;
        case char_X:
            return tokeniser_lower_name;
        case char_space:
            return tokeniser_start;
        case char_e:
            return tokeniser_lower_name;
        case char_l:
            return tokeniser_lower_name;
        case char_m:
            return tokeniser_lower_name;
        case char_d:
            return tokeniser_lower_name;
        case char_u:
            return tokeniser_lower_name;
        };
    case tokeniser_0:
        switch (ch) {
        case char_equals:
            return tokeniser_start;
        case char_0:
            return -1;
        case char_newline:
            return tokeniser_start;
        case char_close_parens:
            return tokeniser_start;
        case char_open_parens:
            return tokeniser_start;
        case char_g:
            return -1;
        case char_n:
            return -1;
        case char_i:
            return -1;
        case char_s:
            return -1;
        case char_o:
            return -1;
        case char_p:
            return -1;
        case char_x:
            return tokeniser_0x;
        case char_X:
            return -1;
        case char_space:
            return tokeniser_start;
        case char_e:
            return -1;
        case char_l:
            return -1;
        case char_m:
            return -1;
        case char_d:
            return -1;
        case char_u:
            return -1;
        };
    case tokeniser_start:
        switch (ch) {
        case char_equals:
            return tokeniser_start;
        case char_0:
            return tokeniser_0;
        case char_newline:
            return tokeniser_start;
        case char_close_parens:
            return tokeniser_start;
        case char_open_parens:
            return tokeniser_start;
        case char_g:
            return tokeniser_lower_name;
        case char_n:
            return tokeniser_lower_name;
        case char_i:
            return tokeniser_i;
        case char_s:
            return tokeniser_lower_name;
        case char_o:
            return tokeniser_o;
        case char_p:
            return tokeniser_p;
        case char_x:
            return tokeniser_lower_name;
        case char_X:
            return tokeniser_upper_name;
        case char_space:
            return tokeniser_start;
        case char_e:
            return tokeniser_e;
        case char_l:
            return tokeniser_l;
        case char_m:
            return tokeniser_m;
        case char_d:
            return tokeniser_lower_name;
        case char_u:
            return tokeniser_lower_name;
        };
    };

    return -1;
}

static int make_tokeniser_states(struct u8x1m chars, struct u8x1m* states) {
    enum tokeniser state = tokeniser_start;
    for (int i = 0; i < chars.length; ++i) {
        enum elm_char ch = chars.items[i];
        int result = tokeniser_state_machine(state, ch);
        if (result < 0) {
            return result;
        }
        result = u8x1m_append(result, states);
        if (result != 0) {
            return result;
        }
    }

    return 0;
}

int format(FILE* input_file, FILE* output_file, struct Memory* memory) {
    int result = read_raw(input_file, &memory->raw);
    if (result != 0) {
        return result;
    }

    result = parse_chars(&memory->raw, &memory->chars);
    if (result != 0) {
        return result;
    }

    return make_tokeniser_states(memory->chars, &memory->tokeniser_state);
}
