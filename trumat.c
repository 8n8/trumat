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

uint8_t ELM_CHARS[256] = {
     0, // 0
     0, // 1 
     0, // 2
     0, // 3
     0, // 4
     0, // 5
     0, // 6
     0, // 7
     0, // 8
     0, // 9
     1, // 10
     0, // 11
     0, // 12
     0, // 13
     0, // 14
     0, // 15
     0, // 16
     0, // 17
     0, // 18
     0, // 19
     0, // 20
     0, // 21
     0, // 22
     0, // 23
     0, // 24
     0, // 25
     0, // 26
     0, // 27
     0, // 28
     0, // 29
     0, // 30
     0, // 31
     Space_ch, // 32
     ExclamationMark_ch, // 33
     DoubleQuote_ch, // 34
     Hash_ch, // 35
     Dollar_ch, // 36
     Percent_ch, // 37
     Ampersand_ch, // 38
     SingleQuote_ch, // 39
     OpenParenthesis_ch, // 40
     CloseParenthesis_ch, // 41
     Star_ch, // 42
     Plus_ch, // 43
     1, // 44
     1, // 45
     1, // 46
     1, // 47
     1, // 48
     1, // 49
     1, // 50
     1, // 51
     1, // 52
     1, // 53
     1, // 54
     1, // 55
     1, // 56
     1, // 57
     1, // 58
     1, // 59
     1, // 60
     1, // 61
     1, // 62
     1, // 63
     1, // 64
     1, // 65
     1, // 66
     1, // 67
     1, // 68
     1, // 69
     1, // 70
     1, // 71
     1, // 72
     1, // 73
     1, // 74
     1, // 75
     1, // 76
     1, // 77
     1, // 78
     1, // 79
     1, // 80
     1, // 81
     1, // 82
     1, // 83
     1, // 84
     1, // 85
     1, // 86
     1, // 87
     1, // 88
     1, // 89
     1, // 90
     1, // 91
     1, // 92
     1, // 93
     1, // 94
     1, // 95
     1, // 96
     1, // 97
     1, // 98
     1, // 99
     1, // 100
     1, // 101 
     1, // 102
     1, // 103
     1, // 104
     1, // 105
     1, // 106
     1, // 107
     1, // 108
     1, // 109
     1, // 110
     1, // 111
     1, // 112
     1, // 113
     1, // 114
     1, // 115
     1, // 116
     1, // 117
     1, // 118
     1, // 119
     1, // 120
     1, // 121
     1, // 122
     1, // 123
     1, // 124
     1, // 125
     1, // 126
     0, // 127
     0, // 128
     0, // 129
     0, // 130
     0, // 131
     0, // 132
     0, // 133
     0, // 134
     0, // 135
     0, // 136
     0, // 137
     0, // 138
     0, // 139
     0, // 140
     0, // 141
     0, // 142
     0, // 143
     0, // 144
     0, // 145
     0, // 146
     0, // 147
     0, // 148
     0, // 149
     0, // 150
     0, // 151
     0, // 152
     0, // 153
     0, // 154
     0, // 155
     0, // 156
     0, // 157
     0, // 158
     0, // 159
     0, // 160
     0, // 161
     0, // 162
     0, // 163
     0, // 164
     0, // 165
     0, // 166
     0, // 167
     0, // 168
     0, // 169
     0, // 170
     0, // 171
     0, // 172
     0, // 173
     0, // 174
     0, // 175
     0, // 176
     0, // 177
     0, // 178
     0, // 179
     0, // 180
     0, // 181
     0, // 182
     0, // 183
     0, // 184
     0, // 185
     0, // 186
     0, // 187
     0, // 188
     0, // 189
     0, // 190
     0, // 191
     0, // 192
     0, // 193
     0, // 194
     0, // 195
     0, // 196
     0, // 197
     0, // 198
     0, // 199
     0, // 200
     0, // 201 
     0, // 202
     0, // 203
     0, // 204
     0, // 205
     0, // 206
     0, // 207
     0, // 208
     0, // 209
     0, // 210
     0, // 211
     0, // 212
     0, // 213
     0, // 214
     0, // 215
     0, // 216
     0, // 217
     0, // 218
     0, // 219
     0, // 220
     0, // 221
     0, // 222
     0, // 223
     0, // 224
     0, // 225
     0, // 226
     0, // 227
     0, // 228
     0, // 229
     0, // 230
     0, // 231
     0, // 232
     0, // 233
     0, // 234
     0, // 235
     0, // 236
     0, // 237
     0, // 238
     0, // 239
     0, // 240
     0, // 241
     0, // 242
     0, // 243
     0, // 244
     0, // 245
     0, // 246
     0, // 247
     0, // 248
     0, // 249
     0, // 250
     0, // 251
     0, // 252
     0, // 253
     0, // 254
     0} // 255

uint8_t CHAR_OK[256] = {
     0, // 0
     0, // 1 
     0, // 2
     0, // 3
     0, // 4
     0, // 5
     0, // 6
     0, // 7
     0, // 8
     0, // 9
     1, // 10
     0, // 11
     0, // 12
     0, // 13
     0, // 14
     0, // 15
     0, // 16
     0, // 17
     0, // 18
     0, // 19
     0, // 20
     0, // 21
     0, // 22
     0, // 23
     0, // 24
     0, // 25
     0, // 26
     0, // 27
     0, // 28
     0, // 29
     0, // 30
     0, // 31
     1, // 32
     1, // 33
     1, // 34
     1, // 35
     1, // 36
     1, // 37
     1, // 38
     1, // 39
     1, // 40
     1, // 41
     1, // 42
     1, // 43
     1, // 44
     1, // 45
     1, // 46
     1, // 47
     1, // 48
     1, // 49
     1, // 50
     1, // 51
     1, // 52
     1, // 53
     1, // 54
     1, // 55
     1, // 56
     1, // 57
     1, // 58
     1, // 59
     1, // 60
     1, // 61
     1, // 62
     1, // 63
     1, // 64
     1, // 65
     1, // 66
     1, // 67
     1, // 68
     1, // 69
     1, // 70
     1, // 71
     1, // 72
     1, // 73
     1, // 74
     1, // 75
     1, // 76
     1, // 77
     1, // 78
     1, // 79
     1, // 80
     1, // 81
     1, // 82
     1, // 83
     1, // 84
     1, // 85
     1, // 86
     1, // 87
     1, // 88
     1, // 89
     1, // 90
     1, // 91
     1, // 92
     1, // 93
     1, // 94
     1, // 95
     1, // 96
     1, // 97
     1, // 98
     1, // 99
     1, // 100
     1, // 101 
     1, // 102
     1, // 103
     1, // 104
     1, // 105
     1, // 106
     1, // 107
     1, // 108
     1, // 109
     1, // 110
     1, // 111
     1, // 112
     1, // 113
     1, // 114
     1, // 115
     1, // 116
     1, // 117
     1, // 118
     1, // 119
     1, // 120
     1, // 121
     1, // 122
     1, // 123
     1, // 124
     1, // 125
     1, // 126
     0, // 127
     0, // 128
     0, // 129
     0, // 130
     0, // 131
     0, // 132
     0, // 133
     0, // 134
     0, // 135
     0, // 136
     0, // 137
     0, // 138
     0, // 139
     0, // 140
     0, // 141
     0, // 142
     0, // 143
     0, // 144
     0, // 145
     0, // 146
     0, // 147
     0, // 148
     0, // 149
     0, // 150
     0, // 151
     0, // 152
     0, // 153
     0, // 154
     0, // 155
     0, // 156
     0, // 157
     0, // 158
     0, // 159
     0, // 160
     0, // 161
     0, // 162
     0, // 163
     0, // 164
     0, // 165
     0, // 166
     0, // 167
     0, // 168
     0, // 169
     0, // 170
     0, // 171
     0, // 172
     0, // 173
     0, // 174
     0, // 175
     0, // 176
     0, // 177
     0, // 178
     0, // 179
     0, // 180
     0, // 181
     0, // 182
     0, // 183
     0, // 184
     0, // 185
     0, // 186
     0, // 187
     0, // 188
     0, // 189
     0, // 190
     0, // 191
     0, // 192
     0, // 193
     0, // 194
     0, // 195
     0, // 196
     0, // 197
     0, // 198
     0, // 199
     0, // 200
     0, // 201 
     0, // 202
     0, // 203
     0, // 204
     0, // 205
     0, // 206
     0, // 207
     0, // 208
     0, // 209
     0, // 210
     0, // 211
     0, // 212
     0, // 213
     0, // 214
     0, // 215
     0, // 216
     0, // 217
     0, // 218
     0, // 219
     0, // 220
     0, // 221
     0, // 222
     0, // 223
     0, // 224
     0, // 225
     0, // 226
     0, // 227
     0, // 228
     0, // 229
     0, // 230
     0, // 231
     0, // 232
     0, // 233
     0, // 234
     0, // 235
     0, // 236
     0, // 237
     0, // 238
     0, // 239
     0, // 240
     0, // 241
     0, // 242
     0, // 243
     0, // 244
     0, // 245
     0, // 246
     0, // 247
     0, // 248
     0, // 249
     0, // 250
     0, // 251
     0, // 252
     0, // 253
     0, // 254
     0} // 255

struct ElmCharResult make_elm_char(char ch) {
    struct ElmCharResult result;
    switch (ch) {
    case 0:
        result.ok = 0;
        return result;

    
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
        uint8_t ok_char = OK_CHARS[in[i]];
        enum ElmChar elm_char = ELM_CHARS[in[i]];
        if (!ok_char) {
            return -1;
        }
        struct TokenizeAction action = tokenize_step(elm_char, state);
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
