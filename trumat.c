#include "trumat.h"
#include <stddef.h>
#include <stdio.h>

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
        if (column_number == (1 << 16)) {
            return -1;
        }
    }

    return 0;
}

enum Token {
    Module,
};

enum TokenizeState {
    Start_ts,
    StartsWithM_ts,
    InsideName_ts,
};
    
struct TokenizeAction {
    int commit;
    enum Token token;
    enum TokenizeState new_state;
};

enum ElmChar {
    m_elm,
};

struct TokenizeAction tokenize_step(enum ElmChar ch, enum TokenizeState state) {
    struct TokenizeAction result = {.commit = 0, .token = 0, .new_state = 0};
    switch (ch) {
    case m_elm:
        switch (state) {
        case Start_ts:
            result.new_state = StartsWithM_ts;
            return result;

        case StartsWithM_ts:
            result.new_state = InsideName_ts;
            return result;

        case InsideName_ts:
            result.new_state = InsideName_ts;
            return result;
        }
    }

    return result;
}

struct ElmCharResult {
    int ok;
    enum ElmChar elm_char;
};

struct ElmCharResult make_elm_char(char ch) {
    struct ElmCharResult result;
    switch (ch) {
    case 'm':
        result.ok = 1;
        result.elm_char = m_elm;
        return result;
    }

    result.ok = 0;
    return result;
}

int tokenize(
    char in[BIG],
    uint8_t tokens[BIG],
    uint32_t token_start[BIG],
    uint32_t token_end[BIG]) {

    int tokens_i = 0;
    int token_start_i = 0;
    enum TokenizeState state = Start_ts;
    for (int i = 0; i < BIG; ++i) {
        struct ElmCharResult result = make_elm_char(in[i]);
        if (!result.ok) {
            return -1;
        }
        struct TokenizeAction action = tokenize_step(result.elm_char, state);
        state = action.new_state;
        if (!action.commit) {
            continue;
        }

        token_start[tokens_i] = token_start_i;
        token_end[tokens_i] = i;
        token_start_i = i + 1;
        tokens[tokens_i] = action.token;
        ++tokens_i;
    }

    return 0;
}

int format(char in[BIG], char out[BIG], struct Memory* memory) {
    int result = tokenize(
        in,
        memory->tokens,
        memory->token_start,
        memory->token_end);

    printf("%d\n", result);
    return result;
}
