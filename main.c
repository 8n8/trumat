#include <dirent.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>

static void literal_write(uint16_t id);
// The formatter works on top level definitions, so this state should be
// enough to contain everything that is needed to format the largest top
// level definition.
//
// Basic assumptions:
//
// - max 5000 lines of code in a single top-level definition
// - average 60 bytes per line of code
// - average 5 nodes per line
#define MAX_NODES 25000 // 5000 * 5
static uint16_t CHILD[MAX_NODES];
static uint16_t SIBLING[MAX_NODES];
static uint8_t NODE_TYPE[MAX_NODES];
static uint32_t SRC_START[MAX_NODES];
static uint16_t SRC_SIZE[MAX_NODES];
// 0 is the null node
// 1 is the root
#define ROOT 1
// So the first non-root node is 2
static int NUM_NODE = 2;
#define MAX_SRC                                                                \
  1400 * 1000 // Twice the size of the largest Elm file I have seen.
static uint8_t SRC[MAX_SRC];
static uint16_t ROW[MAX_SRC];
static uint16_t COLUMN[MAX_SRC];
static int NUM_SRC = 0;
static int I = -1;
static FILE *IN;
static FILE *OUT;

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

enum node_type {
  EMPTY_NODE,
  MODULE_DECLARATION_NODE,
  UPPER_NAME_NODE,
  LOWER_NAME_NODE,
  MODULE_EXPORTS_NODE,
  FILE_NODE,
  BIND_NODE,
  PLAIN_BASE10_NODE,
};

enum error {
  MODULE_KEYWORD_ERROR,
  MODULE_NAME_ERROR,
  EXPOSING_KEYWORD_ERROR,
  NO_MODULE_DECLARATION_ERROR,
  MODULE_EXPORT_ERROR,
  MODULE_EXPORTS_LEFT_PAREN_ERROR,
  MODULE_EXPORTS_RIGHT_PAREN_ERROR,
  KEYWORD_ERROR,
  AFTER_KEYWORD_ERROR,
  UPPER_NAME_END_ERROR,
  LOWER_NAME_END_ERROR,
  LOWER_NAME_START_ERROR,
  UPPER_NAME_START_ERROR,
  TOP_LEVEL_BIND_NAME_ERROR,
  TOP_LEVEL_BIND_EQUALS_ERROR,
  TOP_LEVEL_BIND_BODY_ERROR,
  CHAR_PARSE_EOF_ERROR,
  CHAR_PARSE_NOT_MATCH_ERROR,
  ANY_CHAR_PARSE_EOF_ERROR,
  BASE10_START_ERROR,
  BASE10_END_ERROR,
  SRC_EOF_ERROR,
  SRC_FILE_READ_ERROR,
  SRC_TOO_SHORT_ERROR,
};

char *error_to_string(enum error error) {
  switch (error) {
  case MODULE_KEYWORD_ERROR:
    return "keyword 'module'";
  case MODULE_NAME_ERROR:
    return "module name";
  case EXPOSING_KEYWORD_ERROR:
    return "keyword 'exposing'";
  case NO_MODULE_DECLARATION_ERROR:
    return "no module declaration";
  case MODULE_EXPORT_ERROR:
    return "module export";
  case MODULE_EXPORTS_LEFT_PAREN_ERROR:
    return "module exports left paren '('";
  case MODULE_EXPORTS_RIGHT_PAREN_ERROR:
    return "module exports right paren ')'";
  case KEYWORD_ERROR:
    return "keyword";
  case AFTER_KEYWORD_ERROR:
    return "after keyword";
  case UPPER_NAME_END_ERROR:
    return "upper name end";
  case LOWER_NAME_END_ERROR:
    return "lower name end";
  case LOWER_NAME_START_ERROR:
    return "lower name start";
  case UPPER_NAME_START_ERROR:
    return "upper name start";
  case TOP_LEVEL_BIND_NAME_ERROR:
    return "top level bind name";
  case TOP_LEVEL_BIND_EQUALS_ERROR:
    return "top level bind equals";
  case TOP_LEVEL_BIND_BODY_ERROR:
    return "top level bind body";
  case CHAR_PARSE_EOF_ERROR:
    return "char parse EOF";
  case CHAR_PARSE_NOT_MATCH_ERROR:
    return "char parse not match";
  case ANY_CHAR_PARSE_EOF_ERROR:
    return "any char parse EOF";
  case BASE10_START_ERROR:
    return "base10 start";
  case BASE10_END_ERROR:
    return "base10 end";
  case SRC_EOF_ERROR:
    return "src EOF";
  case SRC_FILE_READ_ERROR:
    return "src file read";
  case SRC_TOO_SHORT_ERROR:
    return "src too short";
  }
}

static int increment_src() {
  if (I == MAX_SRC - 1) {
    return SRC_TOO_SHORT_ERROR;
  }
  ++I;
  return 0;
}

static int char_parse(uint8_t c) {
  if (increment_src()) {
    return CHAR_PARSE_EOF_ERROR;
  }
  if (SRC[I] != c) {
    return CHAR_PARSE_NOT_MATCH_ERROR;
  }
  return 0;
}

static int lower_name_parse(uint16_t *id);

static int any_char_parse(uint8_t *c) {
  if (increment_src()) {
    return ANY_CHAR_PARSE_EOF_ERROR;
  }
  *c = SRC[I];
  return 0;
}

static void many_whitespace_parse() {
  while (1) {
    const int before = I;
    uint8_t c;
    if (any_char_parse(&c)) {
      return;
    }
    if (c != ' ' && c != '\n') {
      I = before;
      return;
    }
  }
}

static int is_after_keyword_char(uint8_t c) { return c == ' ' || c == '\n'; }

static int after_keyword_parse() {
  uint8_t c;
  const int result = any_char_parse(&c);
  if (result) {
    return result;
  }
  if (!is_after_keyword_char(c)) {
    return AFTER_KEYWORD_ERROR;
  }
  return 0;
}

static int keyword_parse(char *keyword) {
  const int start = I;
  for (; *keyword != '\0'; ++keyword) {
    if (char_parse(*keyword)) {
      I = start;
      return KEYWORD_ERROR;
    }
  }
  const int end = I;
  const int after_result = after_keyword_parse();
  I = end;
  return after_result;
}

static uint16_t node_init(enum node_type type) {
  const uint16_t id = NUM_NODE;
  ++NUM_NODE;
  NODE_TYPE[id] = type;
  return id;
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

void dbg_siblings() {
  puts("siblings:");
  for (int i = 0; i < NUM_NODE; ++i) {
    printf("%04d ", SIBLING[i]);
  }
  putchar('\n');
}

void dbg_children() {
  puts("children:");

  for (int i = 0; i < NUM_NODE; ++i) {
    printf("%04d ", CHILD[i]);
  }
  putchar('\n');

  for (int i = 0; i < NUM_NODE; ++i) {
    printf("%s ", node_type_to_string(NODE_TYPE[i]));
  }
  putchar('\n');
}

void dbg_ast() {
  for (int i = 0; i < NUM_NODE; ++i) {
    printf("%04d ", i);
  }
  putchar('\n');
  dbg_children();
  dbg_siblings();
}

static int is_subsequent_name_char(uint8_t c) {
  return (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || c == '_' ||
         (c >= '0' && c <= '9');
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

static int top_level_write() {
  fputs("\n\n\n", OUT);
  uint16_t name = CHILD[ROOT];
  literal_write(name);
  fputs(" =\n    ", OUT);
  uint16_t body = SIBLING[name];
  literal_write(body);
  return 0;
}

static int is_after_base10_char(uint8_t c) { return c == ' ' || c == '\n'; }

static int base10_parse(uint16_t *id) {
  const int start = I;
  uint8_t c;
  const int result = any_char_parse(&c);
  if (result) {
    return result;
  }
  if (c < '0' || c > '9') {
    return BASE10_START_ERROR;
  }
  for (any_char_parse(&c); c >= '0' && c <= '9'; any_char_parse(&c)) {
  }
  if (!is_after_base10_char(c)) {
    return BASE10_END_ERROR;
  }
  *id = node_init(PLAIN_BASE10_NODE);
  SRC_START[*id] = start;
  SRC_SIZE[*id] = I - start;
  return 0;
}

static int top_level_parse_help() {
  uint16_t name;
  const int name_result = lower_name_parse(&name);
  if (name_result) {
    return TOP_LEVEL_BIND_NAME_ERROR;
  }
  many_whitespace_parse();
  if (char_parse('=')) {
    return TOP_LEVEL_BIND_EQUALS_ERROR;
  }
  many_whitespace_parse();
  uint16_t body;
  const int body_result = base10_parse(&body);
  if (body_result) {
    return TOP_LEVEL_BIND_BODY_ERROR;
  }
  CHILD[ROOT] = name;
  NODE_TYPE[ROOT] = BIND_NODE;
  SIBLING[name] = body;
  return 0;
}

static int top_level_parse() {
  const int start = I;
  const int result = top_level_parse_help();
  if (result) {
    I = start;
  }
  return result;
}

static int top_level_format() {
  const int parse_result = top_level_parse();
  if (parse_result) {
    return parse_result;
  }
  return top_level_write();
}

static int module_exports_parse_help(uint16_t *id) {
  *id = node_init(MODULE_EXPORTS_NODE);
  if (char_parse('(')) {
    return MODULE_EXPORTS_LEFT_PAREN_ERROR;
  }
  many_whitespace_parse();
  uint16_t export;
  const int export_result = lower_name_parse(&export);
  if (export_result) {
    return MODULE_EXPORT_ERROR;
  }
  many_whitespace_parse();
  if (char_parse(')')) {
    return MODULE_EXPORTS_RIGHT_PAREN_ERROR;
  }
  CHILD[*id] = export;
  return 0;
}

static int module_exports_parse(uint16_t *id) {
  const int start = I;
  const int result = module_exports_parse_help(id);
  if (result) {
    I = start;
  }
  return result;
}

static int is_after_name_char(uint8_t c) {
  return c == ' ' || c == '\n' || c == ')';
}

static int is_upper_name_start_char(uint8_t c) { return c >= 'A' && c <= 'Z'; }

static int upper_name_parse_help(uint16_t *id) {
  const int start = I;
  uint8_t c;
  const int result = any_char_parse(&c);
  if (result) {
    return result;
  }
  if (!is_upper_name_start_char(c)) {
    return UPPER_NAME_START_ERROR;
  }
  for (any_char_parse(&c); is_subsequent_name_char(c); any_char_parse(&c)) {
  }
  if (!is_after_name_char(c)) {
    return UPPER_NAME_END_ERROR;
  }
  *id = node_init(UPPER_NAME_NODE);
  SRC_START[*id] = start;
  SRC_SIZE[*id] = I - start;
  return 0;
}

static int is_first_lower_name_char(uint8_t c) {
  return (c >= 'a' && c <= 'z') || c == '_';
}

static int first_lower_name_char_parse() {
  const int start = I;
  uint8_t c;
  const int result = any_char_parse(&c);
  if (result) {
    return result;
  }
  if (!is_first_lower_name_char(c)) {
    I = start;
    return LOWER_NAME_START_ERROR;
  }
  return 0;
}

static int after_lower_name_char_parse() {
  const int start = I;
  uint8_t c;
  const int result = any_char_parse(&c);
  if (result) {
    return result;
  }
  if (!is_after_name_char(c)) {
    I = start;
    return LOWER_NAME_END_ERROR;
  }
  return 0;
}

static int subsequent_lower_name_char_parse() {
  const int start = I;
  uint8_t c;
  const int result = any_char_parse(&c);
  if (result) {
    return result;
  }
  if (!is_subsequent_name_char(c)) {
    I = start;
    return 1;
  }
  return 0;
}

static int lower_name_parse_help(uint16_t *id) {
  const int start = I;
  const int result = first_lower_name_char_parse();
  if (result) {
    return result;
  }
  while (!subsequent_lower_name_char_parse()) {
  }
  const int end = I;
  const int after_result = after_lower_name_char_parse();
  if (after_result) {
    return after_result;
  }
  I = end;

  *id = node_init(LOWER_NAME_NODE);
  SRC_START[*id] = start;
  SRC_SIZE[*id] = I - start;
  return 0;
}

static int lower_name_parse(uint16_t *id) {
  const int start = I;
  const int result = lower_name_parse_help(id);
  if (result) {
    I = start;
  }
  return result;
}

static int upper_name_parse(uint16_t *id) {
  const int start = I;
  const int result = upper_name_parse_help(id);
  if (result) {
    I = start;
  }
  return result;
}

static int module_declaration_parse_help(uint16_t *id) {
  *id = ROOT;
  NODE_TYPE[*id] = MODULE_DECLARATION_NODE;
  if (keyword_parse("module")) {
    return MODULE_KEYWORD_ERROR;
  }
  many_whitespace_parse();
  uint16_t upper_name;
  if (upper_name_parse(&upper_name)) {
    return MODULE_NAME_ERROR;
  }
  many_whitespace_parse();
  if (keyword_parse("exposing")) {
    return EXPOSING_KEYWORD_ERROR;
  }
  many_whitespace_parse();
  uint16_t exports;
  const int module_exports_result = module_exports_parse(&exports);
  if (module_exports_result) {
    return module_exports_result;
  }
  CHILD[*id] = upper_name;
  SIBLING[upper_name] = exports;
  return 0;
}

static int module_declaration_parse() {
  const int start = I;
  uint16_t module_declaration;
  const int result = module_declaration_parse_help(&module_declaration);
  if (result) {
    I = start;
  }
  return result;
}

static void literal_write(uint16_t id) {
  fwrite(SRC + SRC_START[id], 1, SRC_SIZE[id], OUT);
}

static void exports_write(uint16_t id) {
  fputc('(', OUT);
  const uint16_t export = CHILD[id];
  literal_write(export);
  for (uint16_t sibling = SIBLING[export]; sibling != 0;
       sibling = SIBLING[sibling]) {
    fputs(", ", OUT);
    literal_write(sibling);
  }
  fputc(')', OUT);
}

static void module_declaration_write() {
  const uint16_t id = ROOT;
  uint16_t upper_name = CHILD[id];
  uint16_t exports = SIBLING[upper_name];
  fputs("module ", OUT);
  literal_write(upper_name);
  fputs(" exposing ", OUT);
  exports_write(exports);
}

static int module_declaration_format() {
  const int parse_result = module_declaration_parse();
  if (parse_result) {
    return parse_result;
  }
  dbg_ast();
  module_declaration_write();
  return 0;
}

static int with_in_out_files() {
  NUM_NODE = 2;
  const int declaration_result = module_declaration_format();
  if (declaration_result) {
    return declaration_result;
  }
  many_whitespace_parse();

  while (!feof(IN)) {
    NUM_NODE = 2;

    const int top_level_result = top_level_format();
    if (top_level_result) {
      return top_level_result;
    }
    many_whitespace_parse();
  }

  return 0;
}

const char *tmp_file_name = ".tmp_trumat";

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

static void with_in_file(char *path) {
  OUT = fopen(tmp_file_name, "w");
  if (OUT == NULL) {
    return;
  }
  NUM_SRC = fread(SRC, 1, MAX_SRC, IN);
  if (NUM_SRC == MAX_SRC) {
    fprintf(stderr, "file too large: %s, maximum size is %d bytes\n", path,
            MAX_SRC);
    return;
  }
  calculate_row_numbers();
  calculate_column_numbers();

  dbg_src();
  const int result = with_in_out_files();
  fclose(OUT);
  if (result) {
    fprintf(stderr, "could not format %s: %s\n", path, error_to_string(result));
    return;
  }
  rename(tmp_file_name, path);
}

static void format_file(char *path) {
  IN = fopen(path, "r");
  if (IN == NULL) {
    return;
  }
  with_in_file(path);
  fclose(IN);
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
