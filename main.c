#include <dirent.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#define SRC_OPEN_BLOCK 1

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

#define MAX_NODES 10 * 1000 * 1000
uint32_t PARENT[MAX_NODES];
int NUM_NODES = 0;
uint32_t TEXT_START[MAX_NODES];
uint16_t TEXT_SIZE[MAX_NODES];
uint8_t NODE_TYPE[MAX_NODES];

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
    if (SRC[i] == SRC_OPEN_BLOCK) {
      fputc('{', stdout);
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

enum open_block_char {
  OPEN_BLOCK_CHAR_OTHER,
  OPEN_BLOCK_CHAR_CURLY,
  OPEN_BLOCK_CHAR_HYPHEN,
  OPEN_BLOCK_CHAR_PIPE,
};

const uint8_t classify_open_block_char[256] = {
    OPEN_BLOCK_CHAR_OTHER,  // 0
    OPEN_BLOCK_CHAR_OTHER,  // 1
    OPEN_BLOCK_CHAR_OTHER,  // 2
    OPEN_BLOCK_CHAR_OTHER,  // 3
    OPEN_BLOCK_CHAR_OTHER,  // 4
    OPEN_BLOCK_CHAR_OTHER,  // 5
    OPEN_BLOCK_CHAR_OTHER,  // 6
    OPEN_BLOCK_CHAR_OTHER,  // 7
    OPEN_BLOCK_CHAR_OTHER,  // 8
    OPEN_BLOCK_CHAR_OTHER,  // 9
    OPEN_BLOCK_CHAR_OTHER,  // 10
    OPEN_BLOCK_CHAR_OTHER,  // 11
    OPEN_BLOCK_CHAR_OTHER,  // 12
    OPEN_BLOCK_CHAR_OTHER,  // 13
    OPEN_BLOCK_CHAR_OTHER,  // 14
    OPEN_BLOCK_CHAR_OTHER,  // 15
    OPEN_BLOCK_CHAR_OTHER,  // 16
    OPEN_BLOCK_CHAR_OTHER,  // 17
    OPEN_BLOCK_CHAR_OTHER,  // 18
    OPEN_BLOCK_CHAR_OTHER,  // 19
    OPEN_BLOCK_CHAR_OTHER,  // 20
    OPEN_BLOCK_CHAR_OTHER,  // 21
    OPEN_BLOCK_CHAR_OTHER,  // 22
    OPEN_BLOCK_CHAR_OTHER,  // 23
    OPEN_BLOCK_CHAR_OTHER,  // 24
    OPEN_BLOCK_CHAR_OTHER,  // 25
    OPEN_BLOCK_CHAR_OTHER,  // 26
    OPEN_BLOCK_CHAR_OTHER,  // 27
    OPEN_BLOCK_CHAR_OTHER,  // 28
    OPEN_BLOCK_CHAR_OTHER,  // 29
    OPEN_BLOCK_CHAR_OTHER,  // 30
    OPEN_BLOCK_CHAR_OTHER,  // 31
    OPEN_BLOCK_CHAR_OTHER,  // 32
    OPEN_BLOCK_CHAR_OTHER,  // 33
    OPEN_BLOCK_CHAR_OTHER,  // 34
    OPEN_BLOCK_CHAR_OTHER,  // 35
    OPEN_BLOCK_CHAR_OTHER,  // 36
    OPEN_BLOCK_CHAR_OTHER,  // 37
    OPEN_BLOCK_CHAR_OTHER,  // 38
    OPEN_BLOCK_CHAR_OTHER,  // 39
    OPEN_BLOCK_CHAR_OTHER,  // 40
    OPEN_BLOCK_CHAR_OTHER,  // 41
    OPEN_BLOCK_CHAR_OTHER,  // 42
    OPEN_BLOCK_CHAR_OTHER,  // 43
    OPEN_BLOCK_CHAR_OTHER,  // 44
    OPEN_BLOCK_CHAR_HYPHEN, // 45
    OPEN_BLOCK_CHAR_OTHER,  // 46
    OPEN_BLOCK_CHAR_OTHER,  // 47
    OPEN_BLOCK_CHAR_OTHER,  // 48
    OPEN_BLOCK_CHAR_OTHER,  // 49
    OPEN_BLOCK_CHAR_OTHER,  // 50
    OPEN_BLOCK_CHAR_OTHER,  // 51
    OPEN_BLOCK_CHAR_OTHER,  // 52
    OPEN_BLOCK_CHAR_OTHER,  // 53
    OPEN_BLOCK_CHAR_OTHER,  // 54
    OPEN_BLOCK_CHAR_OTHER,  // 55
    OPEN_BLOCK_CHAR_OTHER,  // 56
    OPEN_BLOCK_CHAR_OTHER,  // 57
    OPEN_BLOCK_CHAR_OTHER,  // 58
    OPEN_BLOCK_CHAR_OTHER,  // 59
    OPEN_BLOCK_CHAR_OTHER,  // 60
    OPEN_BLOCK_CHAR_OTHER,  // 61
    OPEN_BLOCK_CHAR_OTHER,  // 62
    OPEN_BLOCK_CHAR_OTHER,  // 63
    OPEN_BLOCK_CHAR_OTHER,  // 64
    OPEN_BLOCK_CHAR_OTHER,  // 65
    OPEN_BLOCK_CHAR_OTHER,  // 66
    OPEN_BLOCK_CHAR_OTHER,  // 67
    OPEN_BLOCK_CHAR_OTHER,  // 68
    OPEN_BLOCK_CHAR_OTHER,  // 69
    OPEN_BLOCK_CHAR_OTHER,  // 70
    OPEN_BLOCK_CHAR_OTHER,  // 71
    OPEN_BLOCK_CHAR_OTHER,  // 72
    OPEN_BLOCK_CHAR_OTHER,  // 73
    OPEN_BLOCK_CHAR_OTHER,  // 74
    OPEN_BLOCK_CHAR_OTHER,  // 75
    OPEN_BLOCK_CHAR_OTHER,  // 76
    OPEN_BLOCK_CHAR_OTHER,  // 77
    OPEN_BLOCK_CHAR_OTHER,  // 78
    OPEN_BLOCK_CHAR_OTHER,  // 79
    OPEN_BLOCK_CHAR_OTHER,  // 80
    OPEN_BLOCK_CHAR_OTHER,  // 81
    OPEN_BLOCK_CHAR_OTHER,  // 82
    OPEN_BLOCK_CHAR_OTHER,  // 83
    OPEN_BLOCK_CHAR_OTHER,  // 84
    OPEN_BLOCK_CHAR_OTHER,  // 85
    OPEN_BLOCK_CHAR_OTHER,  // 86
    OPEN_BLOCK_CHAR_OTHER,  // 87
    OPEN_BLOCK_CHAR_OTHER,  // 88
    OPEN_BLOCK_CHAR_OTHER,  // 89
    OPEN_BLOCK_CHAR_OTHER,  // 90
    OPEN_BLOCK_CHAR_OTHER,  // 91
    OPEN_BLOCK_CHAR_OTHER,  // 92
    OPEN_BLOCK_CHAR_OTHER,  // 93
    OPEN_BLOCK_CHAR_OTHER,  // 94
    OPEN_BLOCK_CHAR_OTHER,  // 95
    OPEN_BLOCK_CHAR_OTHER,  // 96
    OPEN_BLOCK_CHAR_OTHER,  // 97
    OPEN_BLOCK_CHAR_OTHER,  // 98
    OPEN_BLOCK_CHAR_OTHER,  // 99
    OPEN_BLOCK_CHAR_OTHER,  // 100
    OPEN_BLOCK_CHAR_OTHER,  // 101
    OPEN_BLOCK_CHAR_OTHER,  // 102
    OPEN_BLOCK_CHAR_OTHER,  // 103
    OPEN_BLOCK_CHAR_OTHER,  // 104
    OPEN_BLOCK_CHAR_OTHER,  // 105
    OPEN_BLOCK_CHAR_OTHER,  // 106
    OPEN_BLOCK_CHAR_OTHER,  // 107
    OPEN_BLOCK_CHAR_OTHER,  // 108
    OPEN_BLOCK_CHAR_OTHER,  // 109
    OPEN_BLOCK_CHAR_OTHER,  // 110
    OPEN_BLOCK_CHAR_OTHER,  // 111
    OPEN_BLOCK_CHAR_OTHER,  // 112
    OPEN_BLOCK_CHAR_OTHER,  // 113
    OPEN_BLOCK_CHAR_OTHER,  // 114
    OPEN_BLOCK_CHAR_OTHER,  // 115
    OPEN_BLOCK_CHAR_OTHER,  // 116
    OPEN_BLOCK_CHAR_OTHER,  // 117
    OPEN_BLOCK_CHAR_OTHER,  // 118
    OPEN_BLOCK_CHAR_OTHER,  // 119
    OPEN_BLOCK_CHAR_OTHER,  // 120
    OPEN_BLOCK_CHAR_OTHER,  // 121
    OPEN_BLOCK_CHAR_OTHER,  // 122
    OPEN_BLOCK_CHAR_CURLY,  // 123
    OPEN_BLOCK_CHAR_PIPE,   // 124
    OPEN_BLOCK_CHAR_OTHER,  // 125
    OPEN_BLOCK_CHAR_OTHER,  // 126
    OPEN_BLOCK_CHAR_OTHER,  // 127
    OPEN_BLOCK_CHAR_OTHER,  // 128
    OPEN_BLOCK_CHAR_OTHER,  // 129
    OPEN_BLOCK_CHAR_OTHER,  // 130
    OPEN_BLOCK_CHAR_OTHER,  // 131
    OPEN_BLOCK_CHAR_OTHER,  // 132
    OPEN_BLOCK_CHAR_OTHER,  // 133
    OPEN_BLOCK_CHAR_OTHER,  // 134
    OPEN_BLOCK_CHAR_OTHER,  // 135
    OPEN_BLOCK_CHAR_OTHER,  // 136
    OPEN_BLOCK_CHAR_OTHER,  // 137
    OPEN_BLOCK_CHAR_OTHER,  // 138
    OPEN_BLOCK_CHAR_OTHER,  // 139
    OPEN_BLOCK_CHAR_OTHER,  // 140
    OPEN_BLOCK_CHAR_OTHER,  // 141
    OPEN_BLOCK_CHAR_OTHER,  // 142
    OPEN_BLOCK_CHAR_OTHER,  // 143
    OPEN_BLOCK_CHAR_OTHER,  // 144
    OPEN_BLOCK_CHAR_OTHER,  // 145
    OPEN_BLOCK_CHAR_OTHER,  // 146
    OPEN_BLOCK_CHAR_OTHER,  // 147
    OPEN_BLOCK_CHAR_OTHER,  // 148
    OPEN_BLOCK_CHAR_OTHER,  // 149
    OPEN_BLOCK_CHAR_OTHER,  // 150
    OPEN_BLOCK_CHAR_OTHER,  // 151
    OPEN_BLOCK_CHAR_OTHER,  // 152
    OPEN_BLOCK_CHAR_OTHER,  // 153
    OPEN_BLOCK_CHAR_OTHER,  // 154
    OPEN_BLOCK_CHAR_OTHER,  // 155
    OPEN_BLOCK_CHAR_OTHER,  // 156
    OPEN_BLOCK_CHAR_OTHER,  // 157
    OPEN_BLOCK_CHAR_OTHER,  // 158
    OPEN_BLOCK_CHAR_OTHER,  // 159
    OPEN_BLOCK_CHAR_OTHER,  // 160
    OPEN_BLOCK_CHAR_OTHER,  // 161
    OPEN_BLOCK_CHAR_OTHER,  // 162
    OPEN_BLOCK_CHAR_OTHER,  // 163
    OPEN_BLOCK_CHAR_OTHER,  // 164
    OPEN_BLOCK_CHAR_OTHER,  // 165
    OPEN_BLOCK_CHAR_OTHER,  // 166
    OPEN_BLOCK_CHAR_OTHER,  // 167
    OPEN_BLOCK_CHAR_OTHER,  // 168
    OPEN_BLOCK_CHAR_OTHER,  // 169
    OPEN_BLOCK_CHAR_OTHER,  // 170
    OPEN_BLOCK_CHAR_OTHER,  // 171
    OPEN_BLOCK_CHAR_OTHER,  // 172
    OPEN_BLOCK_CHAR_OTHER,  // 173
    OPEN_BLOCK_CHAR_OTHER,  // 174
    OPEN_BLOCK_CHAR_OTHER,  // 175
    OPEN_BLOCK_CHAR_OTHER,  // 176
    OPEN_BLOCK_CHAR_OTHER,  // 177
    OPEN_BLOCK_CHAR_OTHER,  // 178
    OPEN_BLOCK_CHAR_OTHER,  // 179
    OPEN_BLOCK_CHAR_OTHER,  // 180
    OPEN_BLOCK_CHAR_OTHER,  // 181
    OPEN_BLOCK_CHAR_OTHER,  // 182
    OPEN_BLOCK_CHAR_OTHER,  // 183
    OPEN_BLOCK_CHAR_OTHER,  // 184
    OPEN_BLOCK_CHAR_OTHER,  // 185
    OPEN_BLOCK_CHAR_OTHER,  // 186
    OPEN_BLOCK_CHAR_OTHER,  // 187
    OPEN_BLOCK_CHAR_OTHER,  // 188
    OPEN_BLOCK_CHAR_OTHER,  // 189
    OPEN_BLOCK_CHAR_OTHER,  // 190
    OPEN_BLOCK_CHAR_OTHER,  // 191
    OPEN_BLOCK_CHAR_OTHER,  // 192
    OPEN_BLOCK_CHAR_OTHER,  // 193
    OPEN_BLOCK_CHAR_OTHER,  // 194
    OPEN_BLOCK_CHAR_OTHER,  // 195
    OPEN_BLOCK_CHAR_OTHER,  // 196
    OPEN_BLOCK_CHAR_OTHER,  // 197
    OPEN_BLOCK_CHAR_OTHER,  // 198
    OPEN_BLOCK_CHAR_OTHER,  // 199
    OPEN_BLOCK_CHAR_OTHER,  // 200
    OPEN_BLOCK_CHAR_OTHER,  // 201
    OPEN_BLOCK_CHAR_OTHER,  // 202
    OPEN_BLOCK_CHAR_OTHER,  // 203
    OPEN_BLOCK_CHAR_OTHER,  // 204
    OPEN_BLOCK_CHAR_OTHER,  // 205
    OPEN_BLOCK_CHAR_OTHER,  // 206
    OPEN_BLOCK_CHAR_OTHER,  // 207
    OPEN_BLOCK_CHAR_OTHER,  // 208
    OPEN_BLOCK_CHAR_OTHER,  // 209
    OPEN_BLOCK_CHAR_OTHER,  // 210
    OPEN_BLOCK_CHAR_OTHER,  // 211
    OPEN_BLOCK_CHAR_OTHER,  // 212
    OPEN_BLOCK_CHAR_OTHER,  // 213
    OPEN_BLOCK_CHAR_OTHER,  // 214
    OPEN_BLOCK_CHAR_OTHER,  // 215
    OPEN_BLOCK_CHAR_OTHER,  // 216
    OPEN_BLOCK_CHAR_OTHER,  // 217
    OPEN_BLOCK_CHAR_OTHER,  // 218
    OPEN_BLOCK_CHAR_OTHER,  // 219
    OPEN_BLOCK_CHAR_OTHER,  // 220
    OPEN_BLOCK_CHAR_OTHER,  // 221
    OPEN_BLOCK_CHAR_OTHER,  // 222
    OPEN_BLOCK_CHAR_OTHER,  // 223
    OPEN_BLOCK_CHAR_OTHER,  // 224
    OPEN_BLOCK_CHAR_OTHER,  // 225
    OPEN_BLOCK_CHAR_OTHER,  // 226
    OPEN_BLOCK_CHAR_OTHER,  // 227
    OPEN_BLOCK_CHAR_OTHER,  // 228
    OPEN_BLOCK_CHAR_OTHER,  // 229
    OPEN_BLOCK_CHAR_OTHER,  // 230
    OPEN_BLOCK_CHAR_OTHER,  // 231
    OPEN_BLOCK_CHAR_OTHER,  // 232
    OPEN_BLOCK_CHAR_OTHER,  // 233
    OPEN_BLOCK_CHAR_OTHER,  // 234
    OPEN_BLOCK_CHAR_OTHER,  // 235
    OPEN_BLOCK_CHAR_OTHER,  // 236
    OPEN_BLOCK_CHAR_OTHER,  // 237
    OPEN_BLOCK_CHAR_OTHER,  // 238
    OPEN_BLOCK_CHAR_OTHER,  // 239
    OPEN_BLOCK_CHAR_OTHER,  // 240
    OPEN_BLOCK_CHAR_OTHER,  // 241
    OPEN_BLOCK_CHAR_OTHER,  // 242
    OPEN_BLOCK_CHAR_OTHER,  // 243
    OPEN_BLOCK_CHAR_OTHER,  // 244
    OPEN_BLOCK_CHAR_OTHER,  // 245
    OPEN_BLOCK_CHAR_OTHER,  // 246
    OPEN_BLOCK_CHAR_OTHER,  // 247
    OPEN_BLOCK_CHAR_OTHER,  // 248
    OPEN_BLOCK_CHAR_OTHER,  // 249
    OPEN_BLOCK_CHAR_OTHER,  // 250
    OPEN_BLOCK_CHAR_OTHER,  // 251
    OPEN_BLOCK_CHAR_OTHER,  // 252
    OPEN_BLOCK_CHAR_OTHER,  // 253
    OPEN_BLOCK_CHAR_OTHER,  // 254
    OPEN_BLOCK_CHAR_OTHER,  // 255
};

enum open_block_action {
  OPEN_BLOCK_DO_NOTHING,
  OPEN_BLOCK_WRITE_TOKEN,
};

enum open_block_state {
  OPEN_BLOCK_STATE_OUTSIDE,
  OPEN_BLOCK_STATE_CURLY,
  OPEN_BLOCK_STATE_CURLY_HYPHEN,
};

const uint8_t open_block_state_table[12] = {
    OPEN_BLOCK_STATE_OUTSIDE,      // OPEN_BLOCK_STATE_OUTSIDE * 4 +
                                   // OPEN_BLOCK_CHAR_OTHER = 0
    OPEN_BLOCK_STATE_CURLY,        // OPEN_BLOCK_STATE_OUTSIDE * 4 +
                                   // OPEN_BLOCK_CHAR_CURLY = 1
    OPEN_BLOCK_STATE_OUTSIDE,      // OPEN_BLOCK_STATE_OUTSIDE * 4 +
                                   // OPEN_BLOCK_CHAR_HYPHEN = 2
    OPEN_BLOCK_STATE_OUTSIDE,      // OPEN_BLOCK_STATE_OUTSIDE * 4 +
                                   // OPEN_BLOCK_CHAR_PIPE = 3
    OPEN_BLOCK_STATE_OUTSIDE,      // OPEN_BLOCK_STATE_CURLY * 4 +
                                   // OPEN_BLOCK_CHAR_OTHER = 4
    OPEN_BLOCK_STATE_OUTSIDE,      // OPEN_BLOCK_STATE_CURLY * 4 +
                                   // OPEN_BLOCK_CHAR_CURLY = 5
    OPEN_BLOCK_STATE_CURLY_HYPHEN, // OPEN_BLOCK_STATE_CURLY * 4 +
                                   // OPEN_BLOCK_CHAR_HYPHEN = 6
    OPEN_BLOCK_STATE_OUTSIDE,      // OPEN_BLOCK_STATE_CURLY * 4 +
                                   // OPEN_BLOCK_CHAR_PIPE = 7
    OPEN_BLOCK_STATE_OUTSIDE,      // OPEN_BLOCK_STATE_CURLY_HYPHEN * 4 +
                                   // OPEN_BLOCK_CHAR_OTHER = 8
    OPEN_BLOCK_STATE_OUTSIDE,      // OPEN_BLOCK_STATE_CURLY_HYPHEN * 4 +
                                   // OPEN_BLOCK_CHAR_CURLY = 9
    OPEN_BLOCK_STATE_OUTSIDE,      // OPEN_BLOCK_STATE_CURLY_HYPHEN * 4 +
                                   // OPEN_BLOCK_CHAR_HYPHEN = 10
    OPEN_BLOCK_STATE_OUTSIDE,      // OPEN_BLOCK_STATE_CURLY_HYPHEN * 4 +
                                   // OPEN_BLOCK_CHAR_PIPE = 11
};

const uint8_t open_block_action_table[12] = {
    OPEN_BLOCK_DO_NOTHING, // OPEN_BLOCK_STATE_OUTSIDE * 4 +
                           // OPEN_BLOCK_CHAR_OTHER = 0
    OPEN_BLOCK_DO_NOTHING, // OPEN_BLOCK_STATE_OUTSIDE * 4 +
                           // OPEN_BLOCK_CHAR_CURLY = 1
    OPEN_BLOCK_DO_NOTHING, // OPEN_BLOCK_STATE_OUTSIDE * 4 +
                           // OPEN_BLOCK_CHAR_HYPHEN = 2
    OPEN_BLOCK_DO_NOTHING, // OPEN_BLOCK_STATE_OUTSIDE * 4 +
                           // OPEN_BLOCK_CHAR_PIPE = 3
    OPEN_BLOCK_DO_NOTHING, // OPEN_BLOCK_STATE_CURLY * 4 + OPEN_BLOCK_CHAR_OTHER
                           // = 4
    OPEN_BLOCK_DO_NOTHING, // OPEN_BLOCK_STATE_CURLY * 4 + OPEN_BLOCK_CHAR_CURLY
                           // = 5
    OPEN_BLOCK_DO_NOTHING, // OPEN_BLOCK_STATE_CURLY * 4 +
                           // OPEN_BLOCK_CHAR_HYPHEN = 6
    OPEN_BLOCK_DO_NOTHING, // OPEN_BLOCK_STATE_CURLY * 4 + OPEN_BLOCK_CHAR_PIPE
                           // = 7
    OPEN_BLOCK_WRITE_TOKEN, // OPEN_BLOCK_STATE_CURLY_HYPHEN * 4 +
                            // OPEN_BLOCK_CHAR_OTHER = 8
    OPEN_BLOCK_WRITE_TOKEN, // OPEN_BLOCK_STATE_CURLY_HYPHEN * 4 +
                            // OPEN_BLOCK_CHAR_CURLY = 9
    OPEN_BLOCK_WRITE_TOKEN, // OPEN_BLOCK_STATE_CURLY_HYPHEN * 4 +
                            // OPEN_BLOCK_CHAR_HYPHEN = 10
    OPEN_BLOCK_DO_NOTHING,  // OPEN_BLOCK_STATE_CURLY_HYPHEN * 4 +
                            // OPEN_BLOCK_CHAR_PIPE = 11
};

static void mark_open_block_comment_file(int file_id) {
  const int start = (file_id > 0) ? FILE_SRC_END[file_id - 1] : 0;
  const int end = FILE_SRC_END[file_id];

  enum open_block_state state = OPEN_BLOCK_STATE_OUTSIDE;
  for (int i = start; i < end; ++i) {
    const enum open_block_char c = classify_open_block_char[SRC[i + 2]];
    const int table_key = state * 4 + c;
    const enum open_block_action action = open_block_action_table[table_key];
    SRC[i] = SRC[i] * (action == OPEN_BLOCK_DO_NOTHING) +
             SRC_OPEN_BLOCK * (action == OPEN_BLOCK_WRITE_TOKEN);
    state = open_block_state_table[table_key];
  }
}

static void mark_open_block_comment() {
  for (int i = 0; i < NUM_FILES; ++i) {
    mark_open_block_comment_file(i);
  }
}

int main(int argc, char *argv[]) {
  if (argc != 3 || !string_equal(argv[1], "--overwrite")) {
    fputs(usage, stderr);
    return -1;
  }

  read_src(argv[2]);
  mark_open_block_comment();

  dbg_src();

  return 0;
}
