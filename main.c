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
// Nodes
// -----
//
// A node ID is a 32 bit integer.
//
// There are more nodes than there are text tokens, so let's assume there
// are 10M nodes. That means 10/1.4 = 7 nodes per line of code.
#define MAX_SRC 86 * 1000 * 1000
static uint8_t SRC[MAX_SRC];

struct position {
  int index;
  int file_end;
};

static struct position POSITION;

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

enum node_type {
  MODULE_DECLARATION_NODE,
  UPPER_NAME_NODE,
  LOWER_NAME_NODE,
  MODULE_EXPORTS_NODE,
};

static int position_increment() {
  if (POSITION.index >= POSITION.file_end) {
    return -1;
  }
  ++POSITION.index;
  return 0;
}

static int char_parse(uint8_t c) {
  if (SRC[POSITION.index] != c) {
    return -1;
  }
  return position_increment();
}

static int position_init(int file_index) {
  if (file_index >= NUM_FILES) {
    return -1;
  }
  const int start = (file_index == 0) ? 0 : FILE_ENDS[file_index - 1];
  const int end = FILE_ENDS[file_index];
  POSITION = (struct position){.index = start, .file_end = end};
  return 0;
}

static int node_init(uint32_t *node, enum node_type type) {
  if (NUM_NODES >= MAX_NODES) {
    return -1;
  }
  *node = NUM_NODES;
  ++NUM_NODES;
  NODE_TYPE[*node] = type;
  return 0;
}

static int keyword_parse(char *keyword) {
  const struct position start = POSITION;
  int i = 0;
  for (; keyword[i] != '\0' && char_parse(keyword[i]) == 0; ++i) {
  }
  if (keyword[i] != '\0') {
    POSITION = start;
    return -1;
  }
  return 0;
}

static int any_char_parse(uint8_t *c) {
  *c = SRC[POSITION.index];
  return position_increment();
}

static int is_subsequent_name_char(uint8_t c) {
  return (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || c == '_' ||
         (c >= '0' && c <= '9');
}

static int first_lower_name_char_parse() {
  const struct position start = POSITION;
  uint8_t c;
  if (any_char_parse(&c)) {
    POSITION = start;
    return -1;
  }
  if (c != '_' && (c < 'a' || c > 'z')) {
    POSITION = start;
    return -1;
  }
  return 0;
}

static int subsequent_name_char_parse() {
  const struct position start = POSITION;
  uint8_t c;
  if (any_char_parse(&c)) {
    POSITION = start;
    return -1;
  }
  if (!is_subsequent_name_char(c)) {
    POSITION = start;
    return -1;
  }
  return 0;
}

static int upper_char_parse() {
  const struct position start = POSITION;
  uint8_t c;
  if (any_char_parse(&c)) {
    POSITION = start;
    return -1;
  };
  if (c < 'A' || c > 'Z') {
    POSITION = start;
    return -1;
  }
  return 0;
}

static int lower_name_parse(uint32_t *name) {
  const struct position start = POSITION;
  if (first_lower_name_char_parse()) {
    POSITION = start;
    return -1;
  }
  while (subsequent_name_char_parse() == 0) {}
  const int node_result = node_init(name, LOWER_NAME_NODE);
  if (node_result) {
    POSITION = start;
    return -1;
  }
  uint16_t size = POSITION.index - start.index;
  TEXT_START[*name] = start.index;
  TEXT_SIZE[*name] = size;
  return 0;
}

static int upper_name_parse(uint32_t *name) {
  const struct position start = POSITION;
  if (upper_char_parse()) {
    POSITION = start;
    return -1;
  }
  while (subsequent_name_char_parse() == 0) {}
  const int node_result = node_init(name, UPPER_NAME_NODE);
  if (node_result) {
    POSITION = start;
    return -1;
  }
  uint16_t size = POSITION.index - start.index;
  TEXT_START[*name] = start.index;
  TEXT_SIZE[*name] = size;
  return 0;
}

static void parse_many_whitespace() {
  while (char_parse(' ') == 0) {
  }
}

static int parse_export(uint32_t *export) {
  const struct position start = POSITION;
  if (lower_name_parse(export)) {
    POSITION = start;
    return -1;
  }
  return 0;
}

static int module_exports_parse_help(uint32_t *exports) {
  if (char_parse('(')) {
    return -1;
  }
  if (node_init(exports, MODULE_EXPORTS_NODE)) {
    return -1;
  }

  uint32_t export;
  if (parse_export(&export)) {
    return -1;
  }
  PARENT[export] = *exports;
  return char_parse(')');
}

static int module_exports_parse(uint32_t *exports) {
  const struct position start = POSITION;
  const int result = module_exports_parse_help(exports);
  if (result) {
    POSITION = start;
  }
  return result;
}

static int module_declaration_parse_help(uint32_t *module_declaration) {
  if (keyword_parse("module")) {
    return -1;
  }
  parse_many_whitespace();
  uint32_t name;
  if (upper_name_parse(&name)) {
    return -1;
  }
  parse_many_whitespace();
  if (keyword_parse("exposing")) {
    return -1;
  }
  parse_many_whitespace();
  uint32_t exports;
  if (module_exports_parse(&exports)) {
    return -1;
  }
  uint32_t id;
  if (node_init(&id, MODULE_DECLARATION_NODE)) {
    return -1;
  }
  PARENT[name] = id;
  PARENT[exports] = id;
  return 0;
}

static int module_declaration_parse(uint32_t *module_declaration) {
  const struct position start = POSITION;
  const int result = module_declaration_parse_help(module_declaration);

  if (result) {
    POSITION = start;
  }
  return result;
}

static int parse_file() {
  uint32_t module_declaration;
  return module_declaration_parse(&module_declaration);
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
  for (int i = 0;; ++i) {
    if (position_init(i)) {
      return 0;
    }

    const int file_parse_result = parse_file();
    if (file_parse_result) {
      return file_parse_result;
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
    fprintf(stderr, "formatting failed with exit code %d\n", result);
    return result;
  }

  // dbg_src();

  return 0;
}
