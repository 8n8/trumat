#include "trumat.h"

#define MAX_NESTING 20

enum StateType {
        TopLevelType,
};

struct Parser {
        int nesting;
        int states[MAX_NESTING];
        enum StateType state_types[MAX_NESTING];
};

enum Action {
        MoveBack,
        DoNothing,
        CopyCurrent,
        InsertNewline,
        InsertSpace,
        MoveForward,
        Finish,
        Fail,
};

enum Char {
        ch_m,
        ch_o,
        ch_d,
        ch_u,
        ch_l,
        ch_e,
        ch_space,
        ch_newline,
        ch_X,
        ch_x,
        ch_p,
        ch_s,
        ch_i,
        ch_n,
        ch_g,
        ch_open_parenthesis,
        ch_close_parenthesis,
        ch_equals,
        ch_open_bracket,
        ch_close_bracket,
        ch_after_end,
        ch_other,
};

enum Char classify_char(char ch) {
        switch (ch) {
                case 'm':
                        return ch_m;
                case 'o':
                        return ch_o;
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
                case '\n':
                        return ch_newline;
                case 'X':
                        return ch_X;
                case 'x':
                        return ch_x;
                case 'p':
                        return ch_p;
                case 's':
                        return ch_s;
                case 'i':
                        return ch_i;
                case 'n':
                        return ch_n;
                case 'g':
                        return ch_g;
                case '(':
                        return ch_open_parenthesis;
                case ')':
                        return ch_close_parenthesis;
                case '=':
                        return ch_equals;
                case '[':
                        return ch_open_bracket;
                case ']':
                        return ch_close_bracket;
                case '\0':
                        return ch_after_end;
        }
        return ch_other;
}

int format(char in[BIG], char out[BIG], struct Memory* memory) {
        struct Parser parser;
        init_parser(&parser);

        calculate_row_numbers(in, memory->row_numbers);
        calculate_column_numbers(in, memory->column_numbers);

        int i = 0;
        int j = 0;
        while (1) {
                int row = memory->row_numbers[i];
                int column = memory->column_numbers[i];
                enum Char ch = classify_char(in[i]);
                enum Action action = step(&parser, ch, row, column);
                switch (action) {
                case MoveBack:
                        --i;
                        continue;
                case DoNothing:
                        continue;
                case CopyCurrent:
                        out[j] = in[i];
                        ++i;
                        ++j;
                        continue;
                case InsertNewline:
                        out[j] = '\n';
                        ++j;
                        continue;
                case InsertSpace:
                        out[j] = ' ';
                        ++j;
                        continue;
                case MoveForward:
                        ++i;
                        continue;
                case Finish:
                        return 0;
                case Fail:
                        return -1;
                }
        }
}
