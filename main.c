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
// Let's assume there are 10M nodes. That means 10/1.4 = 7 nodes per line
// of code.
#define MAX_SRC 86 * 1000 * 1000
static uint8_t SRC[MAX_SRC];

struct position {
  int index;
  int file_end;
};

enum node_type {
  MODULE_DECLARATION_NODE,
  UPPER_NAME_NODE,
  LOWER_NAME_NODE,
  MODULE_EXPORTS_NODE,
  FILE_NODE,
  BIND_NODE,
  PLAIN_BASE10_NODE,
  EMPTY_NODE,
};

static struct position POSITION;

#define MAX_PATH 2100 * 1000
static uint8_t PATH[MAX_PATH];

#define MAX_FILES 8250
static uint32_t FILE_ENDS[MAX_FILES];
static uint16_t PATH_END[MAX_FILES];
static uint32_t FILE_NODE_ID[MAX_FILES];
static int NUM_FILES = 0;

#define MAX_NODES 10 * 1000 * 1000
static uint32_t PARENT[MAX_NODES];
static uint32_t TEXT_START[MAX_NODES];
static uint16_t TEXT_SIZE[MAX_NODES];
static uint8_t NODE_TYPE[MAX_NODES] = {EMPTY_NODE};
// The first node (index 0) means the node has no parent
static int NUM_NODES = 1;

static void get_path(uint32_t file_id, char path[256]) {
  const int start = (file_id == 0) ? 0 : PATH_END[file_id - 1];
  const int end = PATH_END[file_id];

  for (int i = start; i < end; ++i) {
    path[i - start] = PATH[i];
  }
  path[end - start] = '\0';
}

static uint32_t find_child(uint32_t parent, enum node_type type) {
  for (int i = 0; i < NUM_NODES; ++i) {
    if (PARENT[i] == parent && NODE_TYPE[i] == type) {
      return i;
    }
  }
  fprintf(stderr, "could not find child of %d\n", parent);
  exit(-1);
}

static void literal_format(FILE *handle, uint32_t literal) {
  for (int i = 0; i < TEXT_SIZE[literal]; ++i) {
    fputc(SRC[TEXT_START[literal] + i], handle);
  }
}

static void module_exports_format(FILE *handle, uint32_t exports) {
  fputs("(", handle);
  uint32_t export = find_child(exports, LOWER_NAME_NODE);
  literal_format(handle, export);
  fputs(")", handle);
}

static void module_declaration_format(FILE *handle, uint32_t root) {
  uint32_t module_declaration = find_child(root, MODULE_DECLARATION_NODE);
  uint32_t module_name = find_child(module_declaration, UPPER_NAME_NODE);
  uint32_t exports = find_child(module_declaration, MODULE_EXPORTS_NODE);
  fputs("module ", handle);
  literal_format(handle, module_name);
  fputs(" exposing ", handle);
  module_exports_format(handle, exports);
}

static void top_levels_format(FILE *handle, uint32_t root) {
  uint32_t bind = find_child(root, BIND_NODE);
  uint32_t name = find_child(bind, LOWER_NAME_NODE);
  uint32_t body = find_child(bind, PLAIN_BASE10_NODE);
  literal_format(handle, name);
  fputs(" =\n    ", handle);
  literal_format(handle, body);
}

static void file_handle_format(FILE *handle, uint32_t file_id) {
  uint32_t root = FILE_NODE_ID[file_id];
  module_declaration_format(handle, root);
  fputs("\n\n\n", handle);
  top_levels_format(handle, root);
  fputc('\n', handle);
}

static void file_format(uint32_t file_id) {
  char path[256];
  get_path(file_id, path);

  FILE *out = fopen(path, "w");
  if (out == NULL) {
    fprintf(stderr, "failed to open %s\n", path);
    return;
  }

  file_handle_format(out, file_id);
  fclose(out);
}

static void ast_format() {
  for (int i = 0; i < NUM_FILES; ++i) {
    file_format(i);
  }
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

void dbg_text_nodes() {
  printf("text nodes:\n");
  for (int i = 0; i < NUM_NODES; ++i) {
    if (TEXT_SIZE[i] == 0) {
      continue;
    }
    printf("%03d: ", i);
    for (int j = 0; j < TEXT_SIZE[i]; ++j) {
      printf("%c", SRC[TEXT_START[i] + j]);
    }
    printf("\n");
  }
}

char *node_type_to_string(enum node_type type) {
  switch (type) {
  case MODULE_DECLARATION_NODE:
    return "MODU";
  case UPPER_NAME_NODE:
    return "UPNA";
  case LOWER_NAME_NODE:
    return "LONA";
  case MODULE_EXPORTS_NODE:
    return "EXPO";
  case FILE_NODE:
    return "FILE";
  case BIND_NODE:
    return "BIND";
  case PLAIN_BASE10_NODE:
    return "NUMB";
  case EMPTY_NODE:
    return "NULL";
  }
}

void dbg_files() {
  printf("files:\n");
  for (int i = 0; i < NUM_FILES; ++i) {
    printf("%03d: ", FILE_NODE_ID[i]);
    print_path(i);
  }
}

void dbg_ast() {
  fputs("indexes:  ", stdout);
  for (int i = 0; i < NUM_NODES; ++i) {
    printf("%03d  ", i);
  }
  fputc('\n', stdout);

  fputs("types:   ", stdout);
  for (int i = 0; i < NUM_NODES; ++i) {
    printf("%s ", node_type_to_string(NODE_TYPE[i]));
  }
  fputc('\n', stdout);

  fputs("parents:  ", stdout);
  for (int i = 0; i < NUM_NODES; ++i) {
    printf("%03d  ", PARENT[i]);
  }
  fputc('\n', stdout);
  dbg_text_nodes();
  dbg_files();
  fputc('\n', stdout);
}

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

static uint32_t node_init(enum node_type type) {
  if (NUM_NODES >= MAX_NODES) {
    fprintf(stderr, "not enough node memory\n");
    exit(-1);
  }
  const uint32_t node = NUM_NODES;
  ++NUM_NODES;
  NODE_TYPE[node] = type;
  return node;
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
  while (subsequent_name_char_parse() == 0) {
  }
  *name = node_init(LOWER_NAME_NODE);
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
  while (subsequent_name_char_parse() == 0) {
  }
  *name = node_init(UPPER_NAME_NODE);
  uint16_t size = POSITION.index - start.index;
  TEXT_START[*name] = start.index;
  TEXT_SIZE[*name] = size;
  return 0;
}

static void many_whitespace_parse() {
  while (char_parse(' ') == 0 || char_parse('\n') == 0) {
  }
}

static int export_parse(uint32_t *export) {
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
  *exports = node_init(MODULE_EXPORTS_NODE);

  uint32_t export;
  if (export_parse(&export)) {
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

static int module_declaration_parse_help(uint32_t *id) {
  if (keyword_parse("module")) {
    return -1;
  }
  many_whitespace_parse();
  uint32_t name;
  if (upper_name_parse(&name)) {
    return -1;
  }
  many_whitespace_parse();
  if (keyword_parse("exposing")) {
    return -1;
  }
  many_whitespace_parse();
  uint32_t exports;
  if (module_exports_parse(&exports)) {
    return -1;
  }
  *id = node_init(MODULE_DECLARATION_NODE);
  PARENT[name] = *id;
  PARENT[exports] = *id;
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

static int eof_parse() { return POSITION.index == POSITION.file_end; }

static int digit_parse() {
  const struct position start = POSITION;
  uint8_t c;
  if (any_char_parse(&c) || c < '0' || c > '9') {
    POSITION = start;
    return -1;
  }
  return 0;
}

static int plain_base10_parse_help(uint32_t *number) {
  const struct position start = POSITION;
  if (digit_parse()) {
    return -1;
  }

  while (digit_parse() == 0) {
  }

  *number = node_init(PLAIN_BASE10_NODE);
  TEXT_START[*number] = start.index;
  TEXT_SIZE[*number] = POSITION.index - start.index;
  return 0;
}

static int plain_base10_parse(uint32_t *id) {
  const struct position start = POSITION;
  const int result = plain_base10_parse_help(id);
  if (result) {
    POSITION = start;
  }
  return result;
}

static int bind_parse_help(uint32_t *id) {
  uint32_t name;
  if (lower_name_parse(&name)) {
    return -1;
  }

  many_whitespace_parse();

  if (char_parse('=')) {
    return -1;
  }

  many_whitespace_parse();

  uint32_t body;
  if (plain_base10_parse(&body)) {
    return -1;
  }

  *id = node_init(BIND_NODE);
  PARENT[name] = *id;
  PARENT[body] = *id;
  return 0;
}

static int bind_parse(uint32_t *id) {
  const struct position start = POSITION;
  const int result = bind_parse_help(id);
  if (result) {
    POSITION = start;
  }
  return result;
}

static int file_parse(uint32_t *file) {
  *file = node_init(FILE_NODE);

  uint32_t module_declaration;
  if (module_declaration_parse(&module_declaration)) {
    return -1;
  }
  PARENT[module_declaration] = *file;

  many_whitespace_parse();

  while (1) {
    uint32_t top_level;
    if (bind_parse(&top_level)) {
      break;
    }
    PARENT[top_level] = *file;
  }

  if (eof_parse()) {
    return -1;
  }

  return 0;
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
    fputc(SRC[i], stdout);
  }
  fputc('\n', stdout);
}

void dbg_src() {
  for (int i = 0; i < NUM_FILES; ++i) {
    print_path(i);
    print_elm_module(i);
    fputc('\n', stdout);
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
  for (int i = 0; position_init(i) == 0; ++i) {
    uint32_t file;
    if (file_parse(&file)) {
      return -1;
    }

    FILE_NODE_ID[i] = file;
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
  // dbg_ast();
  ast_format();

  return 0;
}
