#include <dirent.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#define SRC_BLANK 0
#define SRC_MODULE 1

// DATA
// ====
//
// Maximum supported codebase
// --------------------------
//
// Some Elm stats from elm-pages:
//
// - 375 files
// - 65k lines of code
// - 3.9MB of code
//
// So on average there are:
//
// - 60B per line of code
// - 10kB per file
//
// The biggest Elm codebase I have heard of is 700k lines of code. Let's
// make this tool work for double that. So the stats for the largest
// codebase this tool will support is:
//
// - 8250 files
// - 1.4M lines of code
// - 86MB of code
//
// There are four main types of ID:
//
// - source characters (u32)
// - path characters (u32)
// - files (u16)
// - tokens (u32)
// - nodes (u32)
//
//
// Source characters
// -----------------
//
// A source code character ID is a 32 bit integer.
//
//
// Path characters
// ---------------
//
// If there are 256B in a file path on average there are
// 256B x 8250 = 2.1MB of file paths.
//
//
// Files
// -----
//
// A file ID is a 16 bit integer.
//
//
// Tokens
// ------
//
// A token ID is a 32 bit integer.
//
// The tokens are mostly single characters. Let's assume half as many
// tokens as source code bytes. That's 43M tokens. And let's assume 5M
// text tokens.
//
//
// Nodes
// -----
//
// A node ID is a 32 bit integer.
//
// There are more nodes than there are text tokens, so let's assume there
// are 10M nodes. That means 10/1.4 = 7 nodes per line of code.
#define MAX_SRC 86 * 1000 * 1000
uint8_t SRC[MAX_SRC];
#define MAX_MASK 1343750 // 86M / 64
uint64_t LINE_COMMENT_MASK[MAX_MASK];
uint64_t BLOCK_COMMENT_MASK[MAX_MASK];
uint64_t DOC_COMMENT_MASK[MAX_MASK];
uint64_t TRIPLE_QUOTE_MASK[MAX_MASK];
uint64_t DOUBLE_QUOTE_MASK[MAX_MASK];
uint64_t CHAR_MASK[MAX_MASK];

#define MAX_PATH 2100 * 1000
uint8_t PATH[MAX_PATH];

#define MAX_FILES 8250
uint32_t FILE_SRC_END[MAX_FILES];
uint32_t FILE_TOKEN_END[MAX_FILES];
uint16_t PATH_END[MAX_FILES];
int NUM_FILES = 0;

#define MAX_TOKENS 43 * 1000 * 1000
uint8_t TOKENS[MAX_TOKENS];
int NUM_TOKENS = 0;

#define MAX_TEXT_TOKENS 5000 * 1000
uint32_t TEXT_TOKENS[MAX_TEXT_TOKENS]; // IDs of tokens that have text
uint32_t TEXT_TOKEN_START[MAX_TEXT_TOKENS];
uint16_t TEXT_TOKEN_SIZE[MAX_TEXT_TOKENS];
int NUM_TEXT_TOKENS = 0;

#define MAX_NODES 10 * 1000 * 1000
uint32_t PARENT[MAX_NODES];
int NUM_NODES = 0;
uint32_t TEXT_NODES[MAX_TEXT_TOKENS]; // IDs of nodes that have text
uint32_t TEXT_NODE_START[MAX_TEXT_TOKENS];
uint16_t TEXT_NODE_SIZE[MAX_TEXT_TOKENS];
int NUM_TEXT_NODES = 0;

void print_path(int file_id) {
  int start = 0;
  if (file_id > 0) {
    start = PATH_END[file_id - 1];
  }
  for (int i = start; i < PATH_END[file_id]; ++i) {
    fputc(PATH[i], stdout);
  }
  fputc('\n', stdout);
}

enum module_token_state {
  MODULE_STATE_OUTSIDE,
  MODULE_STATE_M,
  MODULE_STATE_MO,
  MODULE_STATE_MOD,
  MODULE_STATE_MODU,
  MODULE_STATE_MODUL,
  MODULE_STATE_MODULE,
};

enum module_token_char {
  MODULE_M,
  MODULE_O,
  MODULE_D,
  MODULE_U,
  MODULE_L,
  MODULE_E,
  MODULE_NEWLINE,
  MODULE_SPACE,
  MODULE_OTHER,
};

const uint8_t classify_module_char[256] = {
  MODULE_OTHER, // 0
  MODULE_OTHER, // 1
  MODULE_OTHER, // 2
  MODULE_OTHER, // 3
  MODULE_OTHER, // 4
  MODULE_OTHER, // 5
  MODULE_OTHER, // 6
  MODULE_OTHER, // 7
  MODULE_OTHER, // 8
  MODULE_OTHER, // 9
  MODULE_NEWLINE, // 10
  MODULE_OTHER, // 11
  MODULE_OTHER, // 12
  MODULE_SPACE, // 13
  MODULE_OTHER, // 14
  MODULE_OTHER, // 15
  MODULE_OTHER, // 16
  MODULE_OTHER, // 17
  MODULE_OTHER, // 18
  MODULE_OTHER, // 19
  MODULE_OTHER, // 20
  MODULE_OTHER, // 21
  MODULE_OTHER, // 22
  MODULE_OTHER, // 23
  MODULE_OTHER, // 24
  MODULE_OTHER, // 25
  MODULE_OTHER, // 26
  MODULE_OTHER, // 27
  MODULE_OTHER, // 28
  MODULE_OTHER, // 29
  MODULE_OTHER, // 30
  MODULE_OTHER, // 31
  MODULE_SPACE, // 32
  MODULE_OTHER, // 33
  MODULE_OTHER, // 34
  MODULE_OTHER, // 35
  MODULE_OTHER, // 36
  MODULE_OTHER, // 37
  MODULE_OTHER, // 38
  MODULE_OTHER, // 39
  MODULE_OTHER, // 40
  MODULE_OTHER, // 41
  MODULE_OTHER, // 42
  MODULE_OTHER, // 43
  MODULE_OTHER, // 44
  MODULE_OTHER, // 45
  MODULE_OTHER, // 46
  MODULE_OTHER, // 47
  MODULE_OTHER, // 48
  MODULE_OTHER, // 49
  MODULE_OTHER, // 50
  MODULE_OTHER, // 51
  MODULE_OTHER, // 52
  MODULE_OTHER, // 53
  MODULE_OTHER, // 54
  MODULE_OTHER, // 55
  MODULE_OTHER, // 56
  MODULE_OTHER, // 57
  MODULE_OTHER, // 58
  MODULE_OTHER, // 59
  MODULE_OTHER, // 60
  MODULE_OTHER, // 61
  MODULE_OTHER, // 62 
  MODULE_OTHER, // 63
  MODULE_OTHER, // 64
  MODULE_OTHER, // 65
  MODULE_OTHER, // 66
  MODULE_OTHER, // 67
  MODULE_OTHER, // 68
  MODULE_OTHER, // 69
  MODULE_OTHER, // 70
  MODULE_OTHER, // 71
  MODULE_OTHER, // 72
  MODULE_OTHER, // 73
  MODULE_OTHER, // 74
  MODULE_OTHER, // 75
  MODULE_OTHER, // 76
  MODULE_OTHER, // 77
  MODULE_OTHER, // 78
  MODULE_OTHER, // 79
  MODULE_OTHER, // 80
  MODULE_OTHER, // 81
  MODULE_OTHER, // 82
  MODULE_OTHER, // 83
  MODULE_OTHER, // 84
  MODULE_OTHER, // 85
  MODULE_OTHER, // 86
  MODULE_OTHER, // 87
  MODULE_OTHER, // 88
  MODULE_OTHER, // 89
  MODULE_OTHER, // 90
  MODULE_OTHER, // 91
  MODULE_OTHER, // 92
  MODULE_OTHER, // 93
  MODULE_OTHER, // 94
  MODULE_OTHER, // 95
  MODULE_OTHER, // 96
  MODULE_OTHER, // 97
  MODULE_OTHER, // 98
  MODULE_OTHER, // 99
  MODULE_D, // 100
  MODULE_E, // 101
  MODULE_OTHER, // 102
  MODULE_OTHER, // 103
  MODULE_OTHER, // 104
  MODULE_OTHER, // 105
  MODULE_OTHER, // 106
  MODULE_OTHER, // 107
  MODULE_L, // 108
  MODULE_M, // 109
  MODULE_OTHER, // 110
  MODULE_O, // 111
  MODULE_OTHER, // 112
  MODULE_OTHER, // 113
  MODULE_OTHER, // 114
  MODULE_OTHER, // 115
  MODULE_OTHER, // 116
  MODULE_U, // 117
  MODULE_OTHER, // 118
  MODULE_OTHER, // 119
  MODULE_OTHER, // 120
  MODULE_OTHER, // 121
  MODULE_OTHER, // 122
  MODULE_OTHER, // 123
  MODULE_OTHER, // 124
  MODULE_OTHER, // 125
  MODULE_OTHER, // 126
  MODULE_OTHER, // 127
  MODULE_OTHER, // 128
  MODULE_OTHER, // 129
  MODULE_OTHER, // 130
  MODULE_OTHER, // 131
  MODULE_OTHER, // 132
  MODULE_OTHER, // 133
  MODULE_OTHER, // 134
  MODULE_OTHER, // 135
  MODULE_OTHER, // 136
  MODULE_OTHER, // 137
  MODULE_OTHER, // 138
  MODULE_OTHER, // 139
  MODULE_OTHER, // 140
  MODULE_OTHER, // 141
  MODULE_OTHER, // 142
  MODULE_OTHER, // 143
  MODULE_OTHER, // 144
  MODULE_OTHER, // 145
  MODULE_OTHER, // 146
  MODULE_OTHER, // 147
  MODULE_OTHER, // 148
  MODULE_OTHER, // 149
  MODULE_OTHER, // 150
  MODULE_OTHER, // 151
  MODULE_OTHER, // 152
  MODULE_OTHER, // 153
  MODULE_OTHER, // 154
  MODULE_OTHER, // 155
  MODULE_OTHER, // 156
  MODULE_OTHER, // 157
  MODULE_OTHER, // 158
  MODULE_OTHER, // 159
  MODULE_OTHER, // 160
  MODULE_OTHER, // 161
  MODULE_OTHER, // 162
  MODULE_OTHER, // 163
  MODULE_OTHER, // 164
  MODULE_OTHER, // 165
  MODULE_OTHER, // 166
  MODULE_OTHER, // 167
  MODULE_OTHER, // 168
  MODULE_OTHER, // 169
  MODULE_OTHER, // 170
  MODULE_OTHER, // 171
  MODULE_OTHER, // 172
  MODULE_OTHER, // 173
  MODULE_OTHER, // 174
  MODULE_OTHER, // 175
  MODULE_OTHER, // 176
  MODULE_OTHER, // 177
  MODULE_OTHER, // 178
  MODULE_OTHER, // 179
  MODULE_OTHER, // 180
  MODULE_OTHER, // 181
  MODULE_OTHER, // 182
  MODULE_OTHER, // 183
  MODULE_OTHER, // 184
  MODULE_OTHER, // 185
  MODULE_OTHER, // 186
  MODULE_OTHER, // 187
  MODULE_OTHER, // 188
  MODULE_OTHER, // 189
  MODULE_OTHER, // 190
  MODULE_OTHER, // 191
  MODULE_OTHER, // 192
  MODULE_OTHER, // 193
  MODULE_OTHER, // 194
  MODULE_OTHER, // 195
  MODULE_OTHER, // 196
  MODULE_OTHER, // 197
  MODULE_OTHER, // 198
  MODULE_OTHER, // 199
  MODULE_OTHER, // 200
  MODULE_OTHER, // 201
  MODULE_OTHER, // 202
  MODULE_OTHER, // 203
  MODULE_OTHER, // 204
  MODULE_OTHER, // 205
  MODULE_OTHER, // 206
  MODULE_OTHER, // 207
  MODULE_OTHER, // 208
  MODULE_OTHER, // 209
  MODULE_OTHER, // 210
  MODULE_OTHER, // 211
  MODULE_OTHER, // 212
  MODULE_OTHER, // 213
  MODULE_OTHER, // 214
  MODULE_OTHER, // 215
  MODULE_OTHER, // 216
  MODULE_OTHER, // 217
  MODULE_OTHER, // 218
  MODULE_OTHER, // 219
  MODULE_OTHER, // 220
  MODULE_OTHER, // 221
  MODULE_OTHER, // 222
  MODULE_OTHER, // 223
  MODULE_OTHER, // 224
  MODULE_OTHER, // 225
  MODULE_OTHER, // 226
  MODULE_OTHER, // 227
  MODULE_OTHER, // 228
  MODULE_OTHER, // 229
  MODULE_OTHER, // 230
  MODULE_OTHER, // 231
  MODULE_OTHER, // 232
  MODULE_OTHER, // 233
  MODULE_OTHER, // 234
  MODULE_OTHER, // 235
  MODULE_OTHER, // 236
  MODULE_OTHER, // 237
  MODULE_OTHER, // 238
  MODULE_OTHER, // 239
  MODULE_OTHER, // 240
  MODULE_OTHER, // 241
  MODULE_OTHER, // 242
  MODULE_OTHER, // 243
  MODULE_OTHER, // 244
  MODULE_OTHER, // 245
  MODULE_OTHER, // 246
  MODULE_OTHER, // 247
  MODULE_OTHER, // 248
  MODULE_OTHER, // 249
  MODULE_OTHER, // 250
  MODULE_OTHER, // 251
  MODULE_OTHER, // 252
  MODULE_OTHER, // 253
  MODULE_OTHER, // 254
  MODULE_OTHER, // 255
};

// The indexes are 9 * state + char
const uint8_t module_state_table[63] = {
  MODULE_STATE_M, // MODULE_STATE_OUTSIDE * 9 + MODULE_M = 0
  MODULE_STATE_OUTSIDE, // MODULE_STATE_OUTSIDE * 9 + MODULE_O = 1
  MODULE_STATE_OUTSIDE, // MODULE_STATE_OUTSIDE * 9 + MODULE_D = 2
  MODULE_STATE_OUTSIDE, // MODULE_STATE_OUTSIDE * 9 + MODULE_U = 3
  MODULE_STATE_OUTSIDE, // MODULE_STATE_OUTSIDE * 9 + MODULE_L = 4
  MODULE_STATE_OUTSIDE, // MODULE_STATE_OUTSIDE * 9 + MODULE_E = 5
  MODULE_STATE_OUTSIDE, // MODULE_STATE_OUTSIDE * 9 + MODULE_NEWLINE = 6
  MODULE_STATE_OUTSIDE, // MODULE_STATE_OUTSIDE * 9 + MODULE_SPACE = 7
  MODULE_STATE_OUTSIDE, // MODULE_STATE_OUTSIDE * 9 + MODULE_OTHER = 8
  MODULE_STATE_OUTSIDE, // MODULE_STATE_M * 9 + MODULE_M = 9
  MODULE_STATE_MO, // MODULE_STATE_M * 9 + MODULE_O = 10
  MODULE_STATE_OUTSIDE, // MODULE_STATE_M * 9 + MODULE_D = 11
  MODULE_STATE_OUTSIDE, // MODULE_STATE_M * 9 + MODULE_U = 12
  MODULE_STATE_OUTSIDE, // MODULE_STATE_M * 9 + MODULE_L = 13
  MODULE_STATE_OUTSIDE, // MODULE_STATE_M * 9 + MODULE_E = 14
  MODULE_STATE_OUTSIDE, // MODULE_STATE_M * 9 + MODULE_NEWLINE = 15
  MODULE_STATE_OUTSIDE, // MODULE_STATE_M * 9 + MODULE_SPACE = 16
  MODULE_STATE_OUTSIDE, // MODULE_STATE_M * 9 + MODULE_OTHER = 17
  MODULE_STATE_OUTSIDE, // MODULE_STATE_MO * 9 + MODULE_M = 18
  MODULE_STATE_OUTSIDE, // MODULE_STATE_MO * 9 + MODULE_O = 19
  MODULE_STATE_MOD, // MODULE_STATE_MO * 9 + MODULE_D = 20
  MODULE_STATE_OUTSIDE, // MODULE_STATE_MO * 9 + MODULE_U = 21
  MODULE_STATE_OUTSIDE, // MODULE_STATE_MO * 9 + MODULE_L = 22
  MODULE_STATE_OUTSIDE, // MODULE_STATE_MO * 9 + MODULE_E = 23
  MODULE_STATE_OUTSIDE, // MODULE_STATE_MO * 9 + MODULE_NEWLINE = 24
  MODULE_STATE_OUTSIDE, // MODULE_STATE_MO * 9 + MODULE_SPACE = 25
  MODULE_STATE_OUTSIDE, // MODULE_STATE_MO * 9 + MODULE_OTHER = 26
  MODULE_STATE_OUTSIDE, // MODULE_STATE_MOD * 9 + MODULE_M = 27
  MODULE_STATE_OUTSIDE, // MODULE_STATE_MOD * 9 + MODULE_O = 28
  MODULE_STATE_OUTSIDE, // MODULE_STATE_MOD * 9 + MODULE_D = 29
  MODULE_STATE_MODU, // MODULE_STATE_MOD * 9 + MODULE_U = 30
  MODULE_STATE_OUTSIDE, // MODULE_STATE_MOD * 9 + MODULE_L = 31
  MODULE_STATE_OUTSIDE, // MODULE_STATE_MOD * 9 + MODULE_E = 32
  MODULE_STATE_OUTSIDE, // MODULE_STATE_MOD * 9 + MODULE_NEWLINE = 33
  MODULE_STATE_OUTSIDE, // MODULE_STATE_MOD * 9 + MODULE_SPACE = 34
  MODULE_STATE_OUTSIDE, // MODULE_STATE_MOD * 9 + MODULE_OTHER = 35
  MODULE_STATE_OUTSIDE, // MODULE_STATE_MODU * 9 + MODULE_M = 36
  MODULE_STATE_OUTSIDE, // MODULE_STATE_MODU * 9 + MODULE_O = 37
  MODULE_STATE_OUTSIDE, // MODULE_STATE_MODU * 9 + MODULE_D = 38
  MODULE_STATE_OUTSIDE, // MODULE_STATE_MODU * 9 + MODULE_U = 39
  MODULE_STATE_MODUL, // MODULE_STATE_MODU * 9 + MODULE_L = 40
  MODULE_STATE_OUTSIDE, // MODULE_STATE_MODU * 9 + MODULE_E = 41
  MODULE_STATE_OUTSIDE, // MODULE_STATE_MODU * 9 + MODULE_NEWLINE = 42
  MODULE_STATE_OUTSIDE, // MODULE_STATE_MODU * 9 + MODULE_SPACE = 43
  MODULE_STATE_OUTSIDE, // MODULE_STATE_MODU * 9 + MODULE_OTHER = 44
  MODULE_STATE_OUTSIDE, // MODULE_STATE_MODUL * 9 + MODULE_M = 45
  MODULE_STATE_OUTSIDE, // MODULE_STATE_MODUL * 9 + MODULE_O = 46
  MODULE_STATE_OUTSIDE, // MODULE_STATE_MODUL * 9 + MODULE_D = 47
  MODULE_STATE_OUTSIDE, // MODULE_STATE_MODUL * 9 + MODULE_U = 48
  MODULE_STATE_OUTSIDE, // MODULE_STATE_MODUL * 9 + MODULE_L = 49
  MODULE_STATE_MODULE, // MODULE_STATE_MODUL * 9 + MODULE_E = 50
  MODULE_STATE_OUTSIDE, // MODULE_STATE_MODUL * 9 + MODULE_NEWLINE = 51
  MODULE_STATE_OUTSIDE, // MODULE_STATE_MODUL * 9 + MODULE_SPACE = 52
  MODULE_STATE_OUTSIDE, // MODULE_STATE_MODUL * 9 + MODULE_OTHER = 53
  MODULE_STATE_OUTSIDE, // MODULE_STATE_MODULE * 9 + MODULE_M = 54
  MODULE_STATE_OUTSIDE, // MODULE_STATE_MODULE * 9 + MODULE_O = 55
  MODULE_STATE_OUTSIDE, // MODULE_STATE_MODULE * 9 + MODULE_D = 56
  MODULE_STATE_OUTSIDE, // MODULE_STATE_MODULE * 9 + MODULE_U = 57
  MODULE_STATE_OUTSIDE, // MODULE_STATE_MODULE * 9 + MODULE_L = 58
  MODULE_STATE_OUTSIDE, // MODULE_STATE_MODULE * 9 + MODULE_E = 59
  MODULE_STATE_OUTSIDE, // MODULE_STATE_MODULE * 9 + MODULE_NEWLINE = 60
  MODULE_STATE_OUTSIDE, // MODULE_STATE_MODULE * 9 + MODULE_SPACE = 61
  MODULE_STATE_OUTSIDE, // MODULE_STATE_MODULE * 9 + MODULE_OTHER = 62
};

void print_elm_module(int file_id) {
  int start = 0;
  if (file_id > 0) {
    start = FILE_SRC_END[file_id - 1];
  }
  for (int i = start; i < FILE_SRC_END[file_id]; ++i) {
    if (SRC[i] == '\n') {
      fputs("\\n", stdout);
      continue;
    }
    if (SRC[i] == SRC_MODULE) {
      fputc('m', stdout);
      continue;
    }
    fputc(SRC[i], stdout);
  }
  fputc('\n', stdout);
}

void dbg_src() {
  for (int i = 0; i < NUM_FILES; ++i) {
    print_path(i);
    print_elm_module(i);
  }
}

static const char *usage = "expecting two arguments:\n"
                           "1. --overwrite to confirm it's OK to recursively "
                           "overwrite all the Elm files in the path\n"
                           "2. the path to format\n"
                           "\n"
                           "for example:\n"
                           "  $ trumat --overwrite Hello.elm\n"
                           "  $ trumat --overwrite some/directory/name\n";

static int string_length(char *s) {
  int n = 0;
  for (; *s != '\0'; ++s) {
    ++n;
  }
  return n;
}

static int string_equal(char *a, char *b) {
  for (; *a == *b && *a != '\0'; ++a, ++b) {
  }
  return *a == '\0' && *b == '\0';
}

static void save_path(char *path) {
  int path_index = 0;
  if (NUM_FILES > 0) {
    path_index = PATH_END[NUM_FILES - 1];
  }

  for (int i = 0; path[i] != '\0'; ++i, ++path_index) {
    if (path_index >= MAX_PATH) {
      fputs("not enough path memory\n", stderr);
      exit(-1);
    }

    PATH[path_index] = path[i];
  }

  PATH_END[NUM_FILES] = path_index;
}

static void save_file(FILE *file) {
  int src_index = 0;
  if (NUM_FILES > 0) {
    src_index = FILE_SRC_END[NUM_FILES - 1];
  }

  int n = fread(SRC + src_index, 1, MAX_SRC - src_index, file);
  if (!feof(file)) {
    fputs("not enough source memory\n", stderr);
    exit(-1);
  }

  FILE_SRC_END[NUM_FILES] = src_index + n;
}

static void read_one_src(char *path) {
  FILE *file = fopen(path, "r");
  if (file == NULL) {
    return;
  }

  save_path(path);
  save_file(file);

  ++NUM_FILES;

  fclose(file);
}

static int is_elm_path(char *path) {
  int n = string_length(path);
  return n > 4 && path[n - 4] == '.' && path[n - 3] == 'e' &&
         path[n - 2] == 'l' && path[n - 1] == 'm';
}

static void make_sub_path(char *left, char *right, char *result) {
  int i = 0;
  for (; left[i] != '\0'; ++i) {
    result[i] = left[i];
  }
  result[i] = '/';
  ++i;
  int j = 0;
  for (; right[j] != '\0'; ++j) {
    result[i + j] = right[j];
  }
  i += j;
  result[i] = '\0';
}

static void read_src(char *path) {
  DIR *directory_handle = opendir(path);
  if (directory_handle == NULL && !is_elm_path(path)) {
    return;
  }

  if (directory_handle == NULL && is_elm_path(path)) {
    read_one_src(path);
    return;
  }

  struct dirent *entry = readdir(directory_handle);
  while (entry != NULL) {
    if (entry->d_name[0] != '.') {
      char sub_path[256];
      make_sub_path(path, entry->d_name, sub_path);
      read_src(sub_path);
    }
    entry = readdir(directory_handle);
  }
  closedir(directory_handle);
}

static void mark_module_token_file(int file_index) {
  int start = 0;
  if (file_index > 0) {
    start = FILE_SRC_END[file_index - 1];
  } 

  int end = FILE_SRC_END[file_index];

  enum module_token_state state = MODULE_STATE_OUTSIDE;
  for (int i = start; i < end; ++i) {
    uint8_t c = classify_module_char[SRC[i]];
    if ((state == MODULE_STATE_MODULE && c == MODULE_SPACE) ||
        (state == MODULE_STATE_MODULE && c == MODULE_NEWLINE)) {

      SRC[i-6] = SRC_MODULE;
    }
    int table_key = state * 9 + c;
    state = module_state_table[table_key];
  }
}

static void mark_module_token() {
  for (int i = 0; i < NUM_FILES; ++i) {
    mark_module_token_file(i);
  }
}

int main(int argc, char *argv[]) {
  if (argc != 3 || !string_equal(argv[1], "--overwrite")) {
    fputs(usage, stderr);
    return -1;
  }

  read_src(argv[2]);

  mark_module_token();
  dbg_src();

  return 0;
}
