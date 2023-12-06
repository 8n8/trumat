#include <dirent.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>

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

enum quote_state {
  QUOTE_OUTSIDE = 0,
  QUOTE_IN_NORMAL_STRING = 1,
  QUOTE_IN_TRIPLE_STRING = 2,
  QUOTE_IN_LINE_COMMENT = 3,
  QUOTE_IN_BLOCK_COMMENT_1 = 4,
  QUOTE_IN_BLOCK_COMMENT_2 = 5,
  QUOTE_IN_BLOCK_COMMENT_3 = 6,
  QUOTE_IN_BLOCK_COMMENT_4 = 7,
  QUOTE_IN_BLOCK_COMMENT_5 = 8,
  QUOTE_IN_DOC_COMMENT_1 = 9,
  QUOTE_IN_DOC_COMMENT_2 = 10,
  QUOTE_IN_DOC_COMMENT_3 = 11,
  QUOTE_IN_DOC_COMMENT_4 = 12,
  QUOTE_IN_DOC_COMMENT_5 = 13,
  QUOTE_IN_CHAR = 14,
};

enum quote_char {
  QUOTE_DOUBLE_QUOTE = 0,
  QUOTE_SINGLE_QUOTE = 1,
  QUOTE_OPEN_CURLY = 2,
  QUOTE_CLOSE_CURLY = 3,
  QUOTE_HYPHEN = 4,
  QUOTE_PIPE = 5,
  QUOTE_NEWLINE = 6,
  QUOTE_OTHER = 7,
};

enum quote_state decode_state(int output) {
  return output & 0x0F;
}

int encode_input(enum quote_state state, enum quote_char ch) {
  return (state << 3) | ch;
}

int main(int argc, char *argv[]) {
  if (argc != 3 || !string_equal(argv[1], "--overwrite")) {
    fputs(usage, stderr);
    return -1;
  }

  read_src(argv[2]);
  dbg_src();

  return 0;
}
