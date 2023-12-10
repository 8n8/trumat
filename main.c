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
static uint8_t SRC[MAX_SRC];
static int SRC_INDEX;
static int FILE_INDEX;

#define MAX_PATH 2100 * 1000
static uint8_t PATH[MAX_PATH];

#define MAX_FILES 8250
static uint32_t FILE_ENDS[MAX_FILES];
static uint16_t PATH_END[MAX_FILES];
static int NUM_FILES = 0;

#define MAX_NODES 10 * 1000 * 1000
uint32_t PARENT[MAX_NODES];
uint32_t TEXT_START[MAX_NODES];
uint16_t TEXT_SIZE[MAX_NODES];
uint8_t NODE_TYPE[MAX_NODES];
int NUM_NODES = 0;

static int parse_keyword(char *keyword) {
  const int start = SRC_INDEX;
  int i = 0;
  for (; keyword[i] != '\0'; ++i) {
    if (SRC[i] != keyword[i]) {
      return -1;
    }
  }
  if (SRC[i] != ' ' && SRC[i] != '\n') {
    SRC_INDEX = start;
    return -1;
  }
  return 0;
}

static void parse_whitespace() {
  for (; SRC[SRC_INDEX] == ' ' || SRC[SRC_INDEX] == '\n'; ++SRC_INDEX) {
  }
}

static int parse_upper_name() {
  const int start = SRC_INDEX;
  if (SRC[SRC_INDEX] < 'A' || SRC[SRC_INDEX] > 'Z') {
    SRC_INDEX = start;
    return -1;
  }
  ++SRC_INDEX;
  for (; SRC[SRC_INDEX] >= 'a' && SRC[SRC_INDEX] <= 'z'; ++SRC_INDEX) {
  }
  return 0;
}

static int parse_module_declaration() {
  const int start = SRC_INDEX;
  int result = parse_keyword("module");
  if (result) {
    SRC_INDEX = start;
    return result;
  }

  parse_whitespace();

  result = parse_upper_name();
  if (result) {
    SRC_INDEX = start;
    return result;
  }

  return 0;
}

static int parse_file() {
  SRC_INDEX = (FILE_INDEX > 0) ? FILE_ENDS[FILE_INDEX - 1] : 0;
  return parse_module_declaration();
}

static void print_path(int file_id) {
  int start = 0;
  if (file_id > 0) {
    start = PATH_END[file_id - 1];
  }
  for (int i = start; i < PATH_END[file_id]; ++i) {
    fputc(PATH[i], stdout);
  }
  fputc('\n', stdout);
}

static void print_elm_module(int file_id) {
  int start = 0;
  if (file_id > 0) {
    start = FILE_ENDS[file_id - 1];
  }
  for (int i = start; i < FILE_ENDS[file_id]; ++i) {
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
    src_index = FILE_ENDS[NUM_FILES - 1];
  }

  int n = fread(SRC + src_index, 1, MAX_SRC - src_index, file);
  if (!feof(file)) {
    fputs("not enough source memory\n", stderr);
    exit(-1);
  }

  FILE_ENDS[NUM_FILES] = src_index + n;
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

static int parse() {
  for (int i = 0; i < NUM_FILES; ++i) {
    FILE_INDEX = i;
    const int result = parse_file();
    if (result) {
      return result;
    }
  }
  return 0;
}

int main(int argc, char *argv[]) {
  if (argc != 3 || !string_equal(argv[1], "--overwrite")) {
    fputs(usage, stderr);
    return -1;
  }

  read_src(argv[2]);
  int result = parse();
  if (result) {
    return result;
  }

  dbg_src();

  return 0;
}
