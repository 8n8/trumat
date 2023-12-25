#include <dirent.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>

// The formatter works on top level definitions, so this state should be
// enough to contain everything that is needed to format the largest top
// level definition.
//
// Basic assumptions:
//
// - max 5000 lines of code in a single top-level definition
// - average 60 bytes per line of code
// - average 5 nodes per line
#define MAX_NODE 25000 // 5000 * 5
static uint16_t CHILD[MAX_NODE];
static uint16_t SIBLING[MAX_NODE];
static uint8_t NODE_TYPE[MAX_NODE];
static uint32_t SRC_START[MAX_NODE];
static uint16_t SRC_SIZE[MAX_NODE];
// 0 is the null node
// 1 is the root
#define ROOT 1
// So the first non-root node is 2
static int NUM_NODE = 2;
// Twice the size of the largest Elm file I have seen.
#define MAX_SRC 1400 * 1000
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
  LITERAL_NODE,
  MODULE_EXPORTS_ALL_NODE,
  MODULE_EXPORTS_EXPLICIT_NODE,
  BIND_NODE,
};

static int is_text_node(enum node_type type) {
  switch (type) {
  case LITERAL_NODE:
    return 1;
  case EMPTY_NODE:
    return 0;
  case MODULE_DECLARATION_NODE:
    return 0;
  case MODULE_EXPORTS_ALL_NODE:
    return 0;
  case MODULE_EXPORTS_EXPLICIT_NODE:
    return 0;
  case BIND_NODE:
    return 0;
  }
}

enum error {
  MODULE_KEYWORD_ERROR,
  STRING_UNICODE_START_ERROR,
  STRING_UNICODE_END_ERROR,
  NORMAL_STRING_START_ERROR,
  DOUBLE_QUOTE_IN_TRIPLE_STRING_ERROR,
  TWO_DOUBLE_QUOTE_IN_TRIPLE_STRING_ERROR,
  TRIPLE_STRING_START_ERROR,
  TRIPLE_STRING_END_ERROR,
  NORMAL_STRING_END_ERROR,
  NORMAL_STRING_ORDINARY_CHAR_ERROR,
  TRIPLE_STRING_ORDINARY_CHAR_ERROR,
  DIGIT_ERROR,
  HEX_ERROR,
  HEX_START_ERROR,
  HEX_END_ERROR,
  FLOAT_START_ERROR,
  FLOAT_DOT_ERROR,
  FIRST_DIGIT_ERROR,
  FLOAT_AFTER_ERROR,
  FLOAT_END_ERROR,
  NOT_HEX_ERROR,
  MODULE_EXPORTS_ALL_LEFT_PAREN_ERROR,
  MODULE_EXPORTS_ALL_DOTS_ERROR,
  MODULE_EXPORTS_ALL_RIGHT_PAREN_ERROR,
  CHUNK_PARSE_ERROR,
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
  LITERAL_WRITE_ERROR,
};

char *error_to_string(enum error error) {
  switch (error) {
  case TWO_DOUBLE_QUOTE_IN_TRIPLE_STRING_ERROR:
    return "two double quote in triple string";
  case DOUBLE_QUOTE_IN_TRIPLE_STRING_ERROR:
    return "double quote in triple string";
  case STRING_UNICODE_START_ERROR:
    return "string unicode start";
  case TRIPLE_STRING_START_ERROR:
    return "triple string start";
  case TRIPLE_STRING_ORDINARY_CHAR_ERROR:
    return "triple string ordinary char";
  case TRIPLE_STRING_END_ERROR:
    return "triple string end";
  case STRING_UNICODE_END_ERROR:
    return "string unicode end";
  case NORMAL_STRING_ORDINARY_CHAR_ERROR:
    return "normal string ordinary char";
  case HEX_ERROR:
    return "hex";
  case NORMAL_STRING_END_ERROR:
    return "normal string end";
  case NORMAL_STRING_START_ERROR:
    return "normal string start";
  case FIRST_DIGIT_ERROR:
    return "first digit";
  case DIGIT_ERROR:
    return "digit";
  case FLOAT_START_ERROR:
    return "float start";
  case FLOAT_DOT_ERROR:
    return "float dot";
  case FLOAT_AFTER_ERROR:
    return "float after";
  case FLOAT_END_ERROR:
    return "float end";
  case HEX_START_ERROR:
    return "hex start";
  case HEX_END_ERROR:
    return "hex end";
  case NOT_HEX_ERROR:
    return "not hex";
  case MODULE_EXPORTS_ALL_LEFT_PAREN_ERROR:
    return "module exports all left paren '('";
  case MODULE_EXPORTS_ALL_DOTS_ERROR:
    return "module exports all dots '..'";
  case MODULE_EXPORTS_ALL_RIGHT_PAREN_ERROR:
    return "module exports all right paren ')'";
  case LITERAL_WRITE_ERROR:
    return "literal write";
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
  case CHUNK_PARSE_ERROR:
    return "chunk parse";
  }
}

static void panic(enum error error) {
  fprintf(stderr, "panic: %s\n", error_to_string(error));
  exit(error);
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
    --I;
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
  if (NUM_NODE == MAX_NODE) {
    panic(SRC_TOO_SHORT_ERROR);
  }
  const uint16_t id = NUM_NODE;
  SIBLING[id] = 0;
  CHILD[id] = 0;
  ++NUM_NODE;
  NODE_TYPE[id] = type;
  return id;
}

char *node_type_to_string(enum node_type type) {
  switch (type) {
  case MODULE_DECLARATION_NODE:
    return "MODU";
  case LITERAL_NODE:
    return "LITN";
  case MODULE_EXPORTS_ALL_NODE:
    return "EXAL";
  case MODULE_EXPORTS_EXPLICIT_NODE:
    return "EXEX";
  case BIND_NODE:
    return "BIND";
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

void dbg_literals() {
  puts("literals:");
  for (int i = 0; i < NUM_NODE; ++i) {
    const uint16_t start = SRC_START[i];
    const uint16_t size = SRC_SIZE[i];
    if (!is_text_node(NODE_TYPE[i])) {
      continue;
    }
    printf("%04d ", i);
    for (int j = start; j < start + size; ++j) {
      if (SRC[j] == '\n') {
        fputs("\\n", stdout);
        continue;
      }
      fputc(SRC[j], stdout);
    }
    putchar('\n');
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
  dbg_literals();
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

static void literal_write(uint16_t id) {
  const int n = fwrite(SRC + SRC_START[id], 1, SRC_SIZE[id], OUT);
  if (n != SRC_SIZE[id]) {
    panic(LITERAL_WRITE_ERROR);
  }
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

static int is_after_number_char(uint8_t c) { return c == ' ' || c == '\n'; }

static int after_number_parse_help() {
  uint8_t c;
  const int result = any_char_parse(&c);
  if (result) {
    return result;
  }
  if (!is_after_number_char(c)) {
    return FLOAT_AFTER_ERROR;
  }
  return 0;
}

static int after_number_parse() {
  const int start = I;
  const int result = after_number_parse_help();
  I = start;
  return result;
}

static int is_after_normal_string_char(uint8_t c) {
  return c == ' ' || c == '\n';
}

static int after_normal_string_parse_help() {
  uint8_t c;
  const int result = any_char_parse(&c);
  if (result) {
    return result;
  }
  if (!is_after_normal_string_char(c)) {
    return FLOAT_AFTER_ERROR;
  }
  return 0;
}

static int after_normal_string_parse() {
  const int start = I;
  const int result = after_normal_string_parse_help();
  I = start;
  return result;
}

static int hex_digit_parse() {
  const int start = I;
  uint8_t c;
  const int result = any_char_parse(&c);
  if (result) {
    return result;
  }
  if ((c < '0' || c > '9') && (c < 'A' || c > 'F') && (c < 'a' || c > 'f')) {
    I = start;
    return HEX_ERROR;
  }
  return 0;
}

static int digit_parse() {
  const int start = I;
  uint8_t c;
  const int result = any_char_parse(&c);
  if (result) {
    return result;
  }
  if (c < '0' || c > '9') {
    I = start;
    return DIGIT_ERROR;
  }
  return 0;
}

static int some_digits_parse() {
  if (digit_parse()) {
    return FIRST_DIGIT_ERROR;
  }
  while (digit_parse() == 0) {
  }
  return 0;
}

static int some_hex_digits_parse() {
  if (hex_digit_parse()) {
    return FIRST_DIGIT_ERROR;
  }
  while (hex_digit_parse() == 0) {
  }
  return 0;
}

static int base10_parse_help(uint16_t *id) {
  const int start = I;
  char_parse('-');
  const int digits_result = some_digits_parse();
  if (digits_result) {
    return digits_result;
  }
  const int end_result = after_number_parse();
  if (end_result) {
    return end_result;
  }
  *id = node_init(LITERAL_NODE);
  SRC_START[*id] = start + 1;
  SRC_SIZE[*id] = I - start;
  return 0;
}

static int base10_parse(uint16_t *id) {
  const int start = I;
  const int result = base10_parse_help(id);
  if (result) {
    I = start;
  }
  return result;
}

static int chunk_parse(char *chunk) {
  const int start = I;
  for (; *chunk != '\0'; ++chunk) {
    if (char_parse(*chunk)) {
      I = start;
      return CHUNK_PARSE_ERROR;
    }
  }
  return 0;
}

static int hex_parse(uint16_t *id) {
  const int start = I;
  char_parse('-');
  if (chunk_parse("0x")) {
    return NOT_HEX_ERROR;
  }
  const int contents_result = some_hex_digits_parse();
  if (contents_result) {
    return contents_result;
  }
  const int end_result = after_number_parse();
  if (end_result) {
    return end_result;
  }
  for (int i = start + 2; i < I + 1; ++i) {
    if (SRC[i] >= 'a' && SRC[i] <= 'f') {
      SRC[i] = SRC[i] - 'a' + 'A';
    }
  }
  *id = node_init(LITERAL_NODE);
  SRC_START[*id] = start + 1;
  SRC_SIZE[*id] = I - start;
  return 0;
}

static int consume_float() {
  const int before_result = some_digits_parse();
  if (before_result) {
    return before_result;
  }
  const int dot_result = char_parse('.');
  if (dot_result) {
    return FLOAT_DOT_ERROR;
  }
  return some_digits_parse();
}

static int plain_float_parse_help(uint16_t *id) {
  const int start = I;
  char_parse('-');
  const int content_result = consume_float();
  if (content_result) {
    return content_result;
  }
  const int end_result = after_number_parse();
  if (end_result) {
    return end_result;
  }
  *id = node_init(LITERAL_NODE);
  SRC_START[*id] = start + 1;
  SRC_SIZE[*id] = I - start;
  return 0;
}

static int plain_float_parse(uint16_t *id) {
  const int start = I;
  const int result = plain_float_parse_help(id);
  if (result) {
    I = start;
  }
  return result;
}

static int exponent_e_parse() {
  if (char_parse('e') == 0) {
    return 0;
  }

  const int result_E = char_parse('E');
  if (result_E) {
    return result_E;
  }

  SRC[I] = 'e';
  return 0;
}

static int exponent_float_parse_help(uint16_t *id) {
  const int start = I;
  char_parse('-');
  const int before_e = consume_float();
  if (before_e) {
    return before_e;
  }
  const int e_result = exponent_e_parse();
  if (e_result) {
    return e_result;
  }
  char_parse('-');
  const int exponent_result = some_digits_parse();
  if (exponent_result) {
    return exponent_result;
  }
  const int end_result = after_number_parse();
  if (end_result) {
    return end_result;
  }
  *id = node_init(LITERAL_NODE);
  SRC_START[*id] = start + 1;
  SRC_SIZE[*id] = I - start;
  return 0;
}

static int exponent_float_parse(uint16_t *id) {
  const int start = I;
  const int result = exponent_float_parse_help(id);
  if (result) {
    I = start;
  }
  return result;
}

static int normal_string_ordinary_char_parse() {
  const int start = I;
  uint8_t c;
  const int result = any_char_parse(&c);
  if (result) {
    return result;
  }
  if (c == '"' || c == '\\') {
    I = start;
    return NORMAL_STRING_ORDINARY_CHAR_ERROR;
  }
  return 0;
}

static int string_unicode_parse_help() {
  if (chunk_parse("\\u{")) {
    return STRING_UNICODE_START_ERROR;
  }
  const int start = I;
  while (hex_digit_parse() == 0) {
  }
  const int end = I;
  if (char_parse('}')) {
    return STRING_UNICODE_END_ERROR;
  }
  for (int i = start + 1; i < end + 1; ++i) {
    if (SRC[i] >= 'a' && SRC[i] <= 'f') {
      SRC[i] = SRC[i] - 'a' + 'A';
    }
  }
  return 0;
}

static int string_unicode_parse() {
  const int start = I;
  const int result = string_unicode_parse_help();
  if (result) {
    I = start;
  }
  return result;
}

static int normal_string_item_parse() {
  if (normal_string_ordinary_char_parse() == 0) {
    return 0;
  }

  if (chunk_parse("\\\\") == 0) {
    return 0;
  }

  if (chunk_parse("\\n") == 0) {
    return 0;
  }

  if (chunk_parse("\\t") == 0) {
    return 0;
  }

  if (string_unicode_parse() == 0) {
    return 0;
  }

  return chunk_parse("\\\"");
}

static int normal_string_parse_help(uint16_t *id) {
  const int start = I;
  if (char_parse('"')) {
    return NORMAL_STRING_START_ERROR;
  }
  while (normal_string_item_parse() == 0) {
  }
  if (char_parse('"')) {
    return NORMAL_STRING_END_ERROR;
  }
  const int end_result = after_normal_string_parse();
  if (end_result) {
    return end_result;
  }
  *id = node_init(LITERAL_NODE);
  SRC_START[*id] = start + 1;
  SRC_SIZE[*id] = I - start;
  return 0;
}

static int normal_string_parse(uint16_t *id) {
  const int start = I;
  const int result = normal_string_parse_help(id);
  if (result) {
    I = start;
  }
  return result;
}

static int triple_string_ordinary_char_parse() {
  const int start = I;
  uint8_t c;
  const int char_result = any_char_parse(&c);
  if (char_result) {
    return char_result;
  }
  if (c == '"') {
    I = start;
    return TRIPLE_STRING_ORDINARY_CHAR_ERROR;
  }
  return 0;
}

static int single_double_quote_in_triple_string_parse() {
  const int start = I;
  if (chunk_parse("\"\"") == 0) {
    I = start;
    return DOUBLE_QUOTE_IN_TRIPLE_STRING_ERROR;
  }
  return char_parse('"');
}

static int two_double_quote_in_triple_string_parse() {
  const int start = I;
  if (chunk_parse("\"\"\"") == 0) {
    I = start;
    return TWO_DOUBLE_QUOTE_IN_TRIPLE_STRING_ERROR;
  }
  return chunk_parse("\"\"");
}

static int triple_string_item_parse() {
  if (triple_string_ordinary_char_parse() == 0) {
    return 0;
  }
  if (single_double_quote_in_triple_string_parse() == 0) {
    return 0;
  }
  return two_double_quote_in_triple_string_parse();
}

static int triple_string_parse_help(uint16_t *id) {
  const int start = I;
  if (chunk_parse("\"\"\"")) {
    return TRIPLE_STRING_START_ERROR;
  }
  while (triple_string_item_parse() == 0) {
  }
  if (chunk_parse("\"\"\"")) {
    return TRIPLE_STRING_END_ERROR;
  }
  *id = node_init(LITERAL_NODE);
  SRC_START[*id] = start + 1;
  SRC_SIZE[*id] = I - start;
  return 0;
}

static int triple_string_parse(uint16_t *id) {
  const int start = I;
  const int result = triple_string_parse_help(id);
  if (result) {
    I = start;
  }
  return result;
}

static int expression_parse(uint16_t *id) {
  if (base10_parse(id) == 0) {
    return 0;
  }

  if (plain_float_parse(id) == 0) {
    return 0;
  }

  if (exponent_float_parse(id) == 0) {
    return 0;
  }

  if (hex_parse(id) == 0) {
    return 0;
  }

  if (normal_string_parse(id) == 0) {
    return 0;
  }

  return triple_string_parse(id);
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
  const int body_result = expression_parse(&body);
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
  // dbg_ast();
  return top_level_write();
}

static int module_exports_explicit_parse_help(uint16_t *id) {
  *id = node_init(MODULE_EXPORTS_EXPLICIT_NODE);
  if (char_parse('(')) {
    return MODULE_EXPORTS_LEFT_PAREN_ERROR;
  }
  many_whitespace_parse();
  uint16_t export;
  if (lower_name_parse(&export)) {
    return MODULE_EXPORT_ERROR;
  }
  CHILD[*id] = export;
  uint16_t previous = export;
  while (1) {
    many_whitespace_parse();
    if (char_parse(',')) {
      break;
    }
    many_whitespace_parse();
    if (lower_name_parse(&export)) {
      break;
    }
    SIBLING[previous] = export;
    previous = export;
  }
  many_whitespace_parse();
  if (char_parse(')')) {
    return MODULE_EXPORTS_RIGHT_PAREN_ERROR;
  }
  return 0;
}

static int module_exports_explicit_parse(uint16_t *id) {
  const int start = I;
  const int result = module_exports_explicit_parse_help(id);
  if (result) {
    I = start;
  }
  return result;
}

static int module_exports_all_parse_help(uint16_t *id) {
  if (char_parse('(')) {
    return MODULE_EXPORTS_ALL_LEFT_PAREN_ERROR;
  }
  many_whitespace_parse();
  if (chunk_parse("..")) {
    return MODULE_EXPORTS_ALL_DOTS_ERROR;
  }
  many_whitespace_parse();
  if (char_parse(')')) {
    return MODULE_EXPORTS_ALL_RIGHT_PAREN_ERROR;
  }
  *id = node_init(MODULE_EXPORTS_ALL_NODE);
  return 0;
}

static int module_exports_all_parse(uint16_t *id) {
  const int start = I;
  const int result = module_exports_all_parse_help(id);
  if (result) {
    I = start;
  }
  return result;
}

static int module_exports_parse_help(uint16_t *id) {
  if (module_exports_explicit_parse(id) == 0) {
    return 0;
  }

  return module_exports_all_parse(id);
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
  return c == ' ' || c == '\n' || c == ')' || c == ',';
}

static int subsequent_name_char_parse() {
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

static int after_name_char_parse() {
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

static int is_first_upper_name_char(uint8_t c) { return c >= 'A' && c <= 'Z'; }

static int first_upper_name_char_parse() {
  const int start = I;
  uint8_t c;
  const int result = any_char_parse(&c);
  if (result) {
    return result;
  }
  if (!is_first_upper_name_char(c)) {
    I = start;
    return LOWER_NAME_START_ERROR;
  }
  return 0;
}

static int upper_name_parse_help(uint16_t *id) {
  const int start = I + 1;
  const int result = first_upper_name_char_parse();
  if (result) {
    return result;
  }
  while (!subsequent_name_char_parse()) {
  }
  const int end = I + 1;
  const int after_result = after_name_char_parse();
  if (after_result) {
    return after_result;
  }
  I = end - 1;
  *id = node_init(LITERAL_NODE);
  SRC_START[*id] = start;
  SRC_SIZE[*id] = end - start;
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

static int lower_name_parse_help(uint16_t *id) {
  const int start = I + 1;
  const int result = first_lower_name_char_parse();
  if (result) {
    return result;
  }
  while (!subsequent_name_char_parse()) {
  }
  const int end = I + 1;
  const int after_result = after_name_char_parse();
  if (after_result) {
    return after_result;
  }
  I = end - 1;

  *id = node_init(LITERAL_NODE);
  SRC_START[*id] = start;
  SRC_SIZE[*id] = end - start;
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

static void explicit_exports_write(uint16_t id) {
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

static void exports_write(uint16_t id) {
  switch (NODE_TYPE[id]) {
  case MODULE_EXPORTS_EXPLICIT_NODE:
    explicit_exports_write(id);
    return;
  case MODULE_EXPORTS_ALL_NODE:
    fputs("(..)", OUT);
    return;
  case EMPTY_NODE:
    fprintf(stderr, "exports_write: empty node\n");
    exit(-1);
  case MODULE_DECLARATION_NODE:
    fprintf(stderr, "exports_write: module declaration node\n");
    exit(-1);
  case LITERAL_NODE:
    fprintf(stderr, "exports_write: literal node\n");
    exit(-1);
  case BIND_NODE:
    fprintf(stderr, "exports_write: bind node\n");
    exit(-1);
  }
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
  // dbg_ast();
  module_declaration_write();
  return 0;
}

static int with_in_out_files() {
  I = -1;
  NUM_NODE = 2;
  const int declaration_result = module_declaration_format();
  if (declaration_result) {
    return declaration_result;
  }
  many_whitespace_parse();

  while (I < NUM_SRC - 1) {
    NUM_NODE = 2;

    const int top_level_result = top_level_format();
    if (top_level_result) {
      return top_level_result;
    }
    many_whitespace_parse();
  }
  fputc('\n', OUT);

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
