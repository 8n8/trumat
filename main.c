#include <dirent.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>

// Twice the size of the largest Elm file I have seen.
#define MAX_SRC 1400 * 1000
static uint8_t SRC[MAX_SRC];
static uint16_t ROW[MAX_SRC];
static uint16_t COLUMN[MAX_SRC];
static int NUM_SRC = 0;
static int I = -1;
static FILE *OUT;

// Basic assumptions:
//
// - max 1.4MB in an Elm module
// - average 60 bytes per line of code
// - average 5 nodes per line
//
// So there is a maximum of 116_665 nodes.

#define MAX_NODES 116665
static int NUM_NODES = 0;

#define MAX_HEX 10000
static uint32_t IS_HEX[MAX_HEX];
static int NUM_HEX = 0;

#define MAX_HAS_EXPONENT 10000
static uint32_t HAS_EXPONENT[MAX_HAS_EXPONENT];
int NUM_HAS_EXPONENT = 0;

#define MAX_HAS_NEGATIVE 10000
static uint32_t HAS_NEGATIVE[MAX_HAS_NEGATIVE];
int NUM_HAS_NEGATIVE = 0;

static void append_is_negative(int node) {
  if (NUM_HAS_NEGATIVE == MAX_HAS_NEGATIVE) {
    fprintf(stderr, "too many nodes, maximum is %d\n", MAX_HAS_NEGATIVE);
    exit(-1);
  }
  HAS_NEGATIVE[NUM_HAS_NEGATIVE] = node;
  ++NUM_HAS_NEGATIVE;
}

static void append_has_exponent(int node) {
  if (NUM_HAS_EXPONENT == MAX_HAS_EXPONENT) {
    fprintf(stderr, "too many nodes, maximum is %d\n", MAX_HAS_EXPONENT);
    exit(-1);
  }
  HAS_EXPONENT[NUM_HAS_EXPONENT] = node;
  ++NUM_HAS_EXPONENT;
}

static void append_is_hex(int node) {
  if (NUM_HEX == MAX_HEX) {
    fprintf(stderr, "too many nodes, maximum is %d\n", MAX_HEX);
    exit(-1);
  }
  IS_HEX[NUM_HEX] = node;
  ++NUM_HEX;
}

#define MAX_HAS_SRC 50 * 1000
static uint32_t HAS_SRC[MAX_HAS_SRC];
static uint32_t SRC_START[MAX_HAS_SRC];
static uint16_t SRC_SIZE[MAX_HAS_SRC];
static int NUM_HAS_SRC = 0;

static void append_has_src(int node, int start, int size) {
  if (NUM_HAS_SRC == MAX_HAS_SRC) {
    fprintf(stderr, "too many nodes, maximum is %d\n", MAX_HAS_SRC);
    exit(-1);
  }
  HAS_SRC[NUM_HAS_SRC] = node;
  SRC_START[NUM_HAS_SRC] = start;
  SRC_SIZE[NUM_HAS_SRC] = size;
  ++NUM_HAS_SRC;
}

static int get_has_src_index(int node) {
  for (int i = 0; i < NUM_HAS_SRC; ++i) {
    if (HAS_SRC[i] == (uint32_t)node) {
      return i;
    }
  }
  fprintf(stderr, "could not find node %d\n", node);
  exit(-1);
}

static void src_write(int node) {
  const int has_src_index = get_has_src_index(node);
  const int start = SRC_START[has_src_index];
  const int size = SRC_SIZE[has_src_index];
  fwrite(SRC + start, 1, size, OUT);
}

static uint8_t hex_to_uppercase(uint8_t c) {
  if (c >= 'a' && c <= 'f') {
    return c - 'a' + 'A';
  }
  return c;
}

static int is_negative(int node) {
  for (int i = 0; i < NUM_HAS_NEGATIVE; ++i) {
    if (HAS_NEGATIVE[i] == (uint32_t)node) {
      return 1;
    }
  }
  return 0;
}

static void hex_write(int node) {
  if (is_negative(node)) {
    fputc('-', OUT);
  }
  fputs("0x", OUT);
  const int src_index = get_has_src_index(node);
  const int start = SRC_START[src_index];
  const int size = SRC_SIZE[src_index];
  if (size % 2 == 1) {
    fputc('0', OUT);
  }
  for (int i = start; i < start + size; ++i) {
    fputc(SRC[i], OUT);
  }
}

static int is_hex(int node) {
  for (int i = 0; i < NUM_HEX; ++i) {
    if (IS_HEX[i] == (uint32_t)node) {
      return 1;
    }
  }
  return 0;
}

static int is_exponent_float(int node) {
  for (int i = 0; i < NUM_HAS_EXPONENT; ++i) {
    if (HAS_EXPONENT[i] == (uint32_t)node) {
      return 1;
    }
  }
  return 0;
}

static void exponent_float_write(int node) {
  src_write(node);
  fputc('e', OUT);
  if (is_negative(node + 1)) {
    fputc('-', OUT);
  }
  src_write(node + 1);
}

static void expression_write(int node) {
  if (is_hex(node)) {
    hex_write(node);
    return;
  }
  if (is_exponent_float(node)) {
    exponent_float_write(node);
    return;
  }
  src_write(node);
}

void dbg_src() {
  for (int i = 0; i < NUM_SRC; ++i) {
    if (SRC[i] == '\n') {
      fputs("\\n", stdout);
      continue;
    }
    fputc(SRC[i], stdout);
  }
  fputc('\n', stdout);
}

static const char *usage = "expecting two arguments:\n"
                           "1. --overwrite to confirm it's OK to recursively "
                           "overwrite all the Elm files in the path\n"
                           "2. the path to format\n"
                           "\n"
                           "for example:\n"
                           "  $ trumat --overwrite Hello.elm\n"
                           "  $ trumat --overwrite some/directory/name\n";

static int string_equal(char *a, char *b) {
  for (; *a == *b && *a != '\0'; ++a, ++b) {
  }
  return *a == '\0' && *b == '\0';
}

static void zero_ast() {
  I = -1;
  NUM_HAS_SRC = 0;
  NUM_NODES = 0;
  NUM_HEX = 0;
  NUM_HAS_EXPONENT = 0;
  NUM_HAS_NEGATIVE = 0;
}

static int get_new_node() {
  if (NUM_NODES == MAX_NODES) {
    fprintf(stderr, "too many nodes, maximum is %d\n", MAX_NODES);
    exit(-1);
  }
  ++NUM_NODES;
  return NUM_NODES - 1;
}

static int is_digit(uint8_t c) { return c >= '0' && c <= '9'; }

static int any_char_parse(uint8_t *c) {
  if (I == NUM_SRC - 1) {
    return -1;
  }
  ++I;
  *c = SRC[I];
  return 0;
}

static int digit_parse() {
  uint8_t digit;
  if (any_char_parse(&digit)) {
    return -1;
  }
  if (!is_digit(digit)) {
    --I;
    return -1;
  }
  return 0;
}

static int char_parse(uint8_t c) {
  if (I == NUM_SRC - 1) {
    return -1;
  }
  ++I;
  if (SRC[I] != c) {
    --I;
    return -1;
  }
  return 0;
}

static int simple_int_parse(int *node) {
  const int start = I;
  char_parse('-');
  if (digit_parse()) {
    I = start;
    return -1;
  }
  while (digit_parse() == 0) {
  }
  *node = get_new_node();
  append_has_src(*node, start + 1, I - start);
  return 0;
}

static int simple_float_parse(int *node) {
  const int start = I;
  char_parse('-');
  while (digit_parse() == 0) {
  }
  if (char_parse('.') != 0) {
    I = start;
    return -1;
  }
  while (digit_parse() == 0) {
  }
  int end = I;
  while (SRC[end] == '0') {
    --end;
  }
  if (SRC[end] == '.') {
    ++end;
  }
  *node = get_new_node();
  append_has_src(*node, start + 1, end - start);
  return 0;
}

static int exponent_float_parse(int *node) {
  const int start = I;
  if (simple_float_parse(node)) {
    return -1;
  }
  if (char_parse('e') != 0) {
    I = start;
    return -1;
  }
  append_has_exponent(*node);

  const int exp_node = get_new_node();
  if (char_parse('-') == 0) {
    append_is_negative(exp_node);
  }
  while (char_parse('0') == 0) {
  }
  const int start_exp = I;
  while (digit_parse() == 0) {
  }
  append_has_src(exp_node, start_exp + 1, I - start_exp);
  return 0;
}

static int float_parse(int *node) {
  if (exponent_float_parse(node) == 0) {
    return 0;
  }
  return simple_float_parse(node);
}

static int chunk_parse(char *chunk) {
  const int start = I;
  for (; *chunk != '\0' && char_parse(*chunk) == 0; ++chunk) {
  }
  if (*chunk != '\0') {
    I = start;
    return -1;
  }
  return 0;
}

static int is_hex_char(uint8_t c) {
  return (c >= '0' && c <= '9') || (c >= 'a' && c <= 'f') ||
         (c >= 'A' && c <= 'F');
}

static int hex_char_parse() {
  uint8_t c;
  if (any_char_parse(&c)) {
    return -1;
  }
  if (!is_hex_char(c)) {
    --I;
    return -1;
  }
  SRC[I] = hex_to_uppercase(SRC[I]);
  return 0;
}

static int hex_int_parse(int *node) {
  const int reset = I;
  *node = get_new_node();
  if (char_parse('-') == 0) {
    append_is_negative(*node);
  }
  if (chunk_parse("0x")) {
    I = reset;
    return -1;
  }
  const int start = I;
  while (hex_char_parse() == 0) {
  }
  append_has_src(*node, start + 1, I - start);
  append_is_hex(*node);
  return 0;
}

static int int_parse(int *node) {
  if (hex_int_parse(node) == 0) {
    return 0;
  }
  return simple_int_parse(node);
}

static int normal_string_char_parse() {
  uint8_t c;
  if (any_char_parse(&c)) {
    return -1;
  }
  if (c == '"' || c == '\\') {
    --I;
    return -1;
  }
  return 0;
}

static int unicode_hex_parse() {
  const int start = I;
  if (chunk_parse("\\u{")) {
    return -1;
  }

  while (hex_char_parse() == 0) {
  }
  if (char_parse('}') != 0) {
    I = start;
    return -1;
  }
  return 0;
}

static int normal_string_item_parse() {
  if (normal_string_char_parse() == 0) {
    return 0;
  }
  if (chunk_parse("\\\"") == 0) {
    return 0;
  }
  if (chunk_parse("\\\\") == 0) {
    return 0;
  }
  return unicode_hex_parse();
}

static int normal_string_parse(int *node) {
  const int start = I;
  if (char_parse('"')) {
    return -1;
  }
  while (normal_string_item_parse() == 0) {
  }
  if (char_parse('"')) {
    I = start;
    return -1;
  }
  *node = get_new_node();
  append_has_src(*node, start + 1, I - start);
  return 0;
}

static int triple_string_parse(int *node) {
  const int start = I;
  if (chunk_parse("\"\"\"\"\"\"") != 0) {
    return -1;
  }
  *node = get_new_node();
  append_has_src(*node, start + 1, I - start);
  return 0;
}

static int expression_parse(int *node) {
  if (float_parse(node) == 0) {
    return 0;
  }
  if (int_parse(node) == 0) {
    return 0;
  }
  if (triple_string_parse(node) == 0) {
    return 0;
  }
  return normal_string_parse(node);
}

static int module_parse(int *node) {
  if (chunk_parse("module X exposing (x)\n\n\nx =\n    ")) {
    return -1;
  }
  return expression_parse(node);
}

static void module_write(int node) {
  fputs("module X exposing (x)\n\n\nx =\n    ", OUT);
  expression_write(node);
  fputc('\n', OUT);
}

static int with_out_file() {
  zero_ast();
  int node;
  if (module_parse(&node)) {
    return -1;
  }
  module_write(node);
  return 0;
}

static void calculate_row_numbers() {
  int row = 0;
  for (int i = 0; i < NUM_SRC; ++i) {
    ROW[i] = row;
    if (SRC[i] == '\n') {
      ++row;
    }
  }
}

static void calculate_column_numbers() {
  int column = 0;
  for (int i = 0; i < NUM_SRC; ++i) {
    COLUMN[i] = column;
    if (SRC[i] == '\n') {
      column = 0;
    } else {
      ++column;
    }
  }
}

static int read_src(char *path) {
  FILE *file = fopen(path, "r");
  if (file == NULL) {
    return -1;
  }
  NUM_SRC = fread(SRC, 1, MAX_SRC, file);
  fclose(file);
  if (NUM_SRC == MAX_SRC) {
    fprintf(stderr, "file too large: %s, maximum size is %d bytes\n", path,
            MAX_SRC);
    return -1;
  }
  return 0;
}

static void format_file(char *path) {
  if (read_src(path)) {
    return;
  }

  calculate_row_numbers();
  calculate_column_numbers();

  OUT = fopen(path, "w");
  if (OUT == NULL) {
    return;
  }
  const int result = with_out_file();
  fclose(OUT);
  if (result) {
    fprintf(stderr, "could not format %s\n", path);
  }
}

static int string_length(char *s) {
  int n = 0;
  for (; *s != '\0'; ++s) {
    ++n;
  }
  return n;
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

static void format_src(char *path) {
  DIR *directory_handle = opendir(path);
  if (directory_handle == NULL && !is_elm_path(path)) {
    return;
  }

  if (directory_handle == NULL && is_elm_path(path)) {
    format_file(path);
    return;
  }

  struct dirent *entry = readdir(directory_handle);
  while (entry != NULL) {
    if (entry->d_name[0] != '.') {
      char sub_path[256];
      make_sub_path(path, entry->d_name, sub_path);
      format_src(sub_path);
    }
    entry = readdir(directory_handle);
  }
  closedir(directory_handle);
}

int main(int argc, char *argv[]) {
  if (argc != 3 || !string_equal(argv[1], "--overwrite")) {
    fputs(usage, stderr);
    return -1;
  }

  format_src(argv[2]);

  return 0;
}
