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

static uint16_t IS_MODULE_EXPORTS_ALL = 0;

static int NUM_MODULE_EXPOSE_ALL_VARIANTS = 0;
#define MAX_MODULE_EXPOSE_ALL_VARIANTS 500
static uint16_t IS_MODULE_EXPOSE_ALL_VARIANTS[MAX_MODULE_EXPOSE_ALL_VARIANTS];

static int NUM_HANGING_BLOCK_COMMENT = 0;
#define MAX_HANGING_BLOCK_COMMENT 200
static uint16_t IS_HANGING_BLOCK_COMMENT[MAX_HANGING_BLOCK_COMMENT];

static int NUM_SINGLE_LINE_BLOCK_COMMENT = 0;
#define MAX_SINGLE_LINE_BLOCK_COMMENT 200
static uint16_t IS_SINGLE_LINE_BLOCK_COMMENT[MAX_SINGLE_LINE_BLOCK_COMMENT];

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
  EMPTY_BLOCK_COMMENT_NODE = 1,
};

static int increment_src() {
  if (I == MAX_SRC - 1) {
    return -1;
  }
  ++I;
  return 0;
}

static int char_parse(uint8_t c) {
  if (increment_src()) {
    return -1;
  }
  if (SRC[I] != c) {
    --I;
    return -1;
  }
  return 0;
}

static int lower_name_parse(uint16_t *id);

static int any_char_parse(uint8_t *c) {
  if (increment_src()) {
    return -1;
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

static int is_after_keyword_char(uint8_t c) {
  return c == ' ' || c == '\n' || c == '{' || c == '-' || c == '(';
}

static int after_keyword_parse() {
  uint8_t c;
  const int result = any_char_parse(&c);
  if (result) {
    return result;
  }
  if (!is_after_keyword_char(c)) {
    return -1;
  }
  return 0;
}

static int keyword_parse(char *keyword) {
  const int start = I;
  for (; *keyword != '\0'; ++keyword) {
    if (char_parse(*keyword)) {
      I = start;
      return -1;
    }
  }
  const int end = I;
  const int after_result = after_keyword_parse();
  I = end;
  return after_result;
}

static void set_hanging_block_comment_node(uint16_t id) {
  if (NUM_HANGING_BLOCK_COMMENT == MAX_HANGING_BLOCK_COMMENT) {
    exit(128);
  }
  IS_HANGING_BLOCK_COMMENT[NUM_HANGING_BLOCK_COMMENT] = id;
  ++NUM_HANGING_BLOCK_COMMENT;
}

static int is_no_docs_node(uint16_t id) { return NODE_TYPE[id] == 0; }

static int is_module_exports_all_node(uint16_t id) {
  return id == IS_MODULE_EXPORTS_ALL;
}

static int is_module_expose_all_variants_node(uint16_t id) {
  for (int i = 0; i < NUM_MODULE_EXPOSE_ALL_VARIANTS; ++i) {
    if (IS_MODULE_EXPOSE_ALL_VARIANTS[i] == id) {
      return 1;
    }
  }
  return 0;
}

static int is_single_line_block_comment_node(uint16_t id) {
  for (int i = 0; i < NUM_SINGLE_LINE_BLOCK_COMMENT; ++i) {
    if (IS_SINGLE_LINE_BLOCK_COMMENT[i] == id) {
      return 1;
    }
  }
  return 0;
}

static int is_hanging_block_comment_node(uint16_t id) {
  for (int i = 0; i < NUM_HANGING_BLOCK_COMMENT; ++i) {
    if (IS_HANGING_BLOCK_COMMENT[i] == id) {
      return 1;
    }
  }
  return 0;
}

static int is_empty_block_comment_node(uint16_t id) {
  return NODE_TYPE[id] == EMPTY_BLOCK_COMMENT_NODE;
}

static uint16_t general_node_init() {
  if (NUM_NODE == MAX_NODE) {
    exit(125);
  }
  const uint16_t id = NUM_NODE;
  SIBLING[id] = 0;
  CHILD[id] = 0;
  NODE_TYPE[id] = 0;
  ++NUM_NODE;
  return id;
}

static uint16_t empty_block_comment_node_init() {
  const uint16_t node = general_node_init();
  NODE_TYPE[node] = EMPTY_BLOCK_COMMENT_NODE;
  return node;
}

static uint16_t single_line_block_comment_node_init() {
  const uint16_t node = general_node_init();
  if (NUM_SINGLE_LINE_BLOCK_COMMENT == MAX_SINGLE_LINE_BLOCK_COMMENT) {
    exit(129);
  }
  IS_SINGLE_LINE_BLOCK_COMMENT[NUM_SINGLE_LINE_BLOCK_COMMENT] = node;
  ++NUM_SINGLE_LINE_BLOCK_COMMENT;
  return node;
}

static uint16_t hanging_block_comment_node_init() {
  const uint16_t node = general_node_init();
  set_hanging_block_comment_node(node);
  return node;
}

static uint16_t module_expose_all_variants_node_init() {
  const uint16_t node = general_node_init();
  if (NUM_MODULE_EXPOSE_ALL_VARIANTS == MAX_MODULE_EXPOSE_ALL_VARIANTS) {
    exit(127);
  }
  IS_MODULE_EXPOSE_ALL_VARIANTS[NUM_MODULE_EXPOSE_ALL_VARIANTS] = node;
  ++NUM_MODULE_EXPOSE_ALL_VARIANTS;
  return node;
}

static uint16_t module_exports_all_node_init() {
  const uint16_t node = general_node_init();
  IS_MODULE_EXPORTS_ALL = node;
  return node;
}

static uint16_t literal_node_init() {
  const uint16_t node = general_node_init();
  return node;
}

static char *node_type_to_string(uint16_t id) {
  if (is_module_expose_all_variants_node(id)) {
    return "MEXA";
  }
  if (is_empty_block_comment_node(id)) {
    return "EBLK";
  }
  if (is_hanging_block_comment_node(id)) {
    return "HBCB";
  }
  if (is_single_line_block_comment_node(id)) {
    return "SBLK";
  }
  if (is_module_exports_all_node(id)) {
    return "EXAL";
  }
  return "####";
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
    printf("%s ", node_type_to_string(i));
  }
  putchar('\n');
}

void dbg_literals() {
  puts("literals:");
  for (int i = 0; i < NUM_NODE; ++i) {
    const uint16_t start = SRC_START[i];
    const uint16_t size = SRC_SIZE[i];
    if (SRC_SIZE[i] == 0) {
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
    exit(126);
  }
}

static void spaces_write(int indent) {
  for (int i = 0; i < indent; ++i) {
    fputc(' ', OUT);
  }
}

static void single_line_block_comment_write(uint16_t id) {
  fputs("{- ", OUT);
  literal_write(CHILD[id]);
  fputs(" -}", OUT);
}

static void hanging_block_comment_write(uint16_t id, int indent) {
  fputs("{-\n", OUT);
  if (SRC_SIZE[CHILD[id]] != 0) {
    spaces_write(indent + 3);
  }
  literal_write(CHILD[id]);
  for (uint16_t sibling = SIBLING[CHILD[id]]; sibling != 0;
       sibling = SIBLING[sibling]) {
    fputc('\n', OUT);
    if (SRC_SIZE[sibling] != 0) {
      spaces_write(indent + 3);
    }
    literal_write(sibling);
  }
  fputc('\n', OUT);
  spaces_write(indent);
  fputs("-}", OUT);
}

static void multiline_compact_block_comment_write(uint16_t id, int indent) {
  fputs("{- ", OUT);
  literal_write(CHILD[id]);
  for (uint16_t sibling = SIBLING[CHILD[id]]; sibling != 0;
       sibling = SIBLING[sibling]) {
    fputc('\n', OUT);
    if (SRC_SIZE[sibling] != 0) {
      spaces_write(indent + 3);
    }
    literal_write(sibling);
  }
  fputc('\n', OUT);
  spaces_write(indent);
  fputs("-}", OUT);
}

static void comment_write(uint16_t id, int indent) {
  if (is_single_line_block_comment_node(id)) {
    single_line_block_comment_write(id);
    return;
  }
  if (is_hanging_block_comment_node(id)) {
    hanging_block_comment_write(id, indent);
    return;
  }
  if (is_empty_block_comment_node(id)) {
    fputs("{--}", OUT);
    return;
  }
  multiline_compact_block_comment_write(id, indent);
}

static void comment_gap_module_declaration(uint16_t id, int is_multiline) {
  if (SIBLING[id] == 0) {
    return;
  }
  if (is_multiline) {
    fputc('\n', OUT);
    spaces_write(4);
    return;
  }
  fputc(' ', OUT);
}

static void comment_gap(uint16_t id, int indent) {
  if (SIBLING[id] == 0) {
    return;
  }
  fputc('\n', OUT);
  spaces_write(indent);
  return;
}

static int comments_are_multiline(uint16_t id) {
  uint16_t comment = CHILD[id];
  for (; comment != 0 && is_single_line_block_comment_node(comment);
       comment = SIBLING[comment]) {
  }
  return comment != 0;
}

static void comments_write_module_declaration(uint16_t id) {
  if (CHILD[id] == 0) {
    return;
  }

  const int is_multiline = comments_are_multiline(id);

  comment_write(CHILD[id], 4);
  comment_gap_module_declaration(CHILD[id], is_multiline);

  for (uint16_t sibling = SIBLING[CHILD[id]]; sibling != 0;
       sibling = SIBLING[sibling]) {
    comment_write(sibling, 4);
    comment_gap_module_declaration(sibling, is_multiline);
  }
}

static void comments_write(uint16_t id, int indent) {
  if (CHILD[id] == 0) {
    return;
  }

  comment_write(CHILD[id], indent);
  comment_gap(CHILD[id], indent);

  for (uint16_t sibling = SIBLING[CHILD[id]]; sibling != 0;
       sibling = SIBLING[sibling]) {
    comment_write(sibling, indent);
    comment_gap(sibling, indent);
  }
}

static int top_level_write() {
  fputs("\n\n\n", OUT);
  uint16_t name = CHILD[ROOT];
  literal_write(name);
  fputs(" =\n    ", OUT);
  uint16_t comment = SIBLING[name];
  comments_write(comment, 4);
  if (CHILD[comment] != 0) {
    fputs("\n    ", OUT);
  }
  uint16_t body = SIBLING[comment];
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
    return -1;
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
    return -1;
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
    return -1;
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
    return -1;
  }
  return 0;
}

static int some_digits_parse() {
  if (digit_parse()) {
    return -1;
  }
  while (digit_parse() == 0) {
  }
  return 0;
}

static int some_hex_digits_parse() {
  if (hex_digit_parse()) {
    return -1;
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
  *id = literal_node_init();
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
      return -1;
    }
  }
  return 0;
}

static int hex_parse(uint16_t *id) {
  const int start = I;
  char_parse('-');
  if (chunk_parse("0x")) {
    return -1;
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
  *id = literal_node_init();
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
    return -1;
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
  *id = literal_node_init();
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
  *id = literal_node_init();
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
    return -1;
  }
  return 0;
}

static int string_unicode_parse_help() {
  if (chunk_parse("\\u{")) {
    return -1;
  }
  const int start = I;
  while (hex_digit_parse() == 0) {
  }
  const int end = I;
  if (char_parse('}')) {
    return -1;
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
    return -1;
  }
  while (normal_string_item_parse() == 0) {
  }
  if (char_parse('"')) {
    return -1;
  }
  const int end_result = after_normal_string_parse();
  if (end_result) {
    return end_result;
  }
  *id = literal_node_init();
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
  if (c == '"' || c == '\\') {
    I = start;
    return -1;
  }
  return 0;
}

static int single_double_quote_in_triple_string_parse() {
  const int start = I;
  if (chunk_parse("\"\"") == 0) {
    I = start;
    return -1;
  }
  return char_parse('"');
}

static int two_double_quote_in_triple_string_parse() {
  const int start = I;
  if (chunk_parse("\"\"\"") == 0) {
    I = start;
    return -1;
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
  if (chunk_parse("\\\"") == 0) {
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
  return two_double_quote_in_triple_string_parse();
}

static int triple_string_parse_help(uint16_t *id) {
  const int start = I;
  if (chunk_parse("\"\"\"")) {
    return -1;
  }
  while (triple_string_item_parse() == 0) {
  }
  if (chunk_parse("\"\"\"")) {
    return -1;
  }
  *id = literal_node_init();
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

static int is_after_name_char(uint8_t c) {
  return c == ' ' || c == '\n' || c == ')' || c == ',' || c == '{' ||
         c == '-' || c == '=' || c == '.';
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
    return -1;
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
    return -1;
  }
  return 0;
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

static int consume_upper_name_chars() {
  const int result = first_upper_name_char_parse();
  if (result) {
    return result;
  }
  while (!subsequent_name_char_parse()) {
  }
  return 0;
}

static int upper_name_parse_help(uint16_t *id) {
  const int start = I + 1;
  const int result = consume_upper_name_chars();
  if (result) {
    return result;
  }
  const int end = I + 1;
  const int after_result = after_name_char_parse();
  if (after_result) {
    return after_result;
  }
  I = end - 1;
  *id = literal_node_init();
  SRC_START[*id] = start;
  SRC_SIZE[*id] = end - start;
  return 0;
}

static int upper_name_parse(uint16_t *id) {
  const int start = I;
  const int result = upper_name_parse_help(id);
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
  if (lower_name_parse(id) == 0) {
    return 0;
  }
  if (upper_name_parse(id) == 0) {
    return 0;
  }
  return triple_string_parse(id);
}

static int line_comment_char_parse() {
  const int start = I;
  uint8_t c;
  const int result = any_char_parse(&c);
  if (result) {
    return result;
  }
  if (c == '\n') {
    I = start;
    return -1;
  }
  return 0;
}

static int is_whitespace(uint8_t c) { return c == ' ' || c == '\n'; }

static void strip_start(uint16_t id) {
  for (; is_whitespace(SRC[SRC_START[id]]) && SRC_SIZE[id] > 0;
       ++SRC_START[id], --SRC_SIZE[id]) {
  }
}

static void strip_end(uint16_t id) {
  for (;
       is_whitespace(SRC[SRC_START[id] + SRC_SIZE[id] - 1]) && SRC_SIZE[id] > 0;
       --SRC_SIZE[id]) {
  }
}

static void strip(uint16_t id) {
  strip_start(id);
  strip_end(id);
}

static int line_comment_parse(uint16_t *id) {
  const int start = I;
  if (chunk_parse("--")) {
    return -1;
  }
  while (line_comment_char_parse() == 0) {
  }
  *id = literal_node_init();
  SRC_START[*id] = start + 1;
  SRC_SIZE[*id] = I - start;
  strip(*id);
  return 0;
}

static int empty_block_comment_parse(uint16_t *id) {
  const int start = I;
  if (chunk_parse("{-")) {
    return -1;
  }
  many_whitespace_parse();
  if (chunk_parse("-}")) {
    I = start;
    return -1;
  }
  *id = empty_block_comment_node_init();
  return 0;
}

static int block_comment_line_item_parse(int *nesting) {
  const int start = I;
  if (chunk_parse("-}") == 0 && *nesting == 1) {
    I = start;
    return -1;
  }
  if (chunk_parse("{-") == 0) {
    ++*nesting;
    return 0;
  }
  if (chunk_parse("-}") == 0) {
    --*nesting;
    return 0;
  }
  uint8_t c;
  const int result = any_char_parse(&c);
  if (result) {
    return result;
  }
  if (c == '\n') {
    I = start;
    return -1;
  }
  return 0;
}

static int end_block_comment_line_parse() {
  const int start = I;
  if (chunk_parse("-}") == 0) {
    I = start;
    return 0;
  }
  if (char_parse('\n') == 0) {
    return 0;
  }
  return -1;
}

static int hanging_block_comment_end_parse() {
  const int start = I;
  while (char_parse(' ') == 0) {
  }
  if (chunk_parse("-}")) {
    I = start;
    return -1;
  }
  return 0;
}

static int block_comment_line_parse(uint16_t *id, int *nesting) {
  const int start = I;
  if (hanging_block_comment_end_parse() == 0) {
    I = start;
    return -1;
  }
  while (block_comment_line_item_parse(nesting) == 0) {
  }
  if (end_block_comment_line_parse()) {
    I = start;
    return -1;
  }
  *id = literal_node_init();
  SRC_START[*id] = start + 1;
  SRC_SIZE[*id] = I - start;
  strip(*id);
  return 0;
}

static int block_comment_is_hanging_parse() {
  while (char_parse(' ') == 0) {
  }
  const int no_newline = char_parse('\n');
  return !no_newline;
}

static int block_comment_contents_parse(uint16_t *id) {
  const int start = I;
  const int is_hanging = block_comment_is_hanging_parse();
  if (is_hanging) {
    *id = hanging_block_comment_node_init();
  } else {
    *id = single_line_block_comment_node_init();
  }
  uint16_t line;
  int nesting = 1;
  if (block_comment_line_parse(&line, &nesting)) {
    I = start;
    return -1;
  }

  CHILD[*id] = line;

  uint16_t previous = line;
  while (block_comment_line_parse(&line, &nesting) == 0) {
    SIBLING[previous] = line;
    previous = line;
    if (is_hanging) {
      set_hanging_block_comment_node(*id);
    }
  }

  return 0;
}

static int non_empty_block_comment_parse_help(uint16_t *id) {
  if (chunk_parse("{-|") == 0) {
    return -1;
  }
  if (chunk_parse("{-")) {
    return -1;
  }
  if (block_comment_contents_parse(id)) {
    return -1;
  }
  many_whitespace_parse();
  if (chunk_parse("-}")) {
    return -1;
  }
  return 0;
}

static int non_empty_block_comment_parse(uint16_t *id) {
  const int start = I;
  const int result = non_empty_block_comment_parse_help(id);
  if (result) {
    I = start;
  }
  return result;
}

static int comment_parse(uint16_t *id) {
  if (line_comment_parse(id) == 0) {
    return 0;
  }

  if (empty_block_comment_parse(id) == 0) {
    return 0;
  }

  return non_empty_block_comment_parse(id);
}

static uint16_t comments_parse() {
  many_whitespace_parse();
  uint16_t item;
  const uint16_t id = general_node_init();
  const int first_result = comment_parse(&item);
  if (first_result) {
    return id;
  }
  CHILD[id] = item;
  many_whitespace_parse();
  uint16_t previous = item;
  while (comment_parse(&item) == 0) {
    SIBLING[previous] = item;
    previous = item;
    many_whitespace_parse();
  }
  return id;
}

static void join_whitespace(uint16_t a, uint16_t b) {
  uint16_t a_node = CHILD[a];
  if (a_node == 0) {
    CHILD[a] = CHILD[b];
    return;
  }
  for (; SIBLING[a_node] != 0; a_node = SIBLING[a_node]) {
  }

  SIBLING[a_node] = CHILD[b];
}

static int top_level_parse_help() {
  uint16_t name;
  const int name_result = lower_name_parse(&name);
  if (name_result) {
    return -1;
  }
  uint16_t pre_equals_whitespace = comments_parse();
  if (char_parse('=')) {
    return -1;
  }
  uint16_t pre_body_whitespace = comments_parse();
  uint16_t body;
  const int body_result = expression_parse(&body);
  if (body_result) {
    return -1;
  }
  CHILD[ROOT] = name;
  join_whitespace(pre_equals_whitespace, pre_body_whitespace);
  SIBLING[name] = pre_equals_whitespace;
  SIBLING[pre_equals_whitespace] = body;
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

#define MAX_EXPORT 1000
uint16_t UNSORTED[MAX_EXPORT];
uint16_t SORTED[MAX_EXPORT];
int NUM_EXPORT;

static int export_less_than(uint16_t a, uint16_t b) {
  int i = 0;
  for (; i < SRC_SIZE[a] && i < SRC_SIZE[b] &&
         SRC[SRC_START[a] + i] == SRC[SRC_START[b] + i];
       ++i) {
  }

  if (i == SRC_SIZE[a] && i == SRC_SIZE[b]) {
    return 0;
  }

  if (i == SRC_SIZE[a] && i < SRC_SIZE[b]) {
    return 1;
  }

  if (i == SRC_SIZE[b] && i < SRC_SIZE[a]) {
    return 0;
  }

  return SRC[SRC_START[a] + i] < SRC[SRC_START[b] + i];
}

static int export_greater_than_or_equal(uint16_t a, uint16_t b) {
  int i = 0;
  for (; i < SRC_SIZE[a] && i < SRC_SIZE[b] &&
         SRC[SRC_START[a] + i] == SRC[SRC_START[b] + i];
       ++i) {
  }

  if (i == SRC_SIZE[a] && i == SRC_SIZE[b]) {
    return 1;
  }

  if (i == SRC_SIZE[a] && i < SRC_SIZE[b]) {
    return 0;
  }

  if (i == SRC_SIZE[b] && i < SRC_SIZE[a]) {
    return 1;
  }

  return SRC[SRC_START[a] + i] >= SRC[SRC_START[b] + i];
}

static uint16_t export_name(uint16_t id) { return SIBLING[CHILD[id]]; }

static void sort_exports_one(int u) {
  if (export_greater_than_or_equal(export_name(UNSORTED[u]),
                                   export_name(SORTED[u - 1]))) {
    SORTED[u] = UNSORTED[u];
    return;
  }

  int insert_at = u;
  for (; insert_at > 0 && export_less_than(export_name(UNSORTED[insert_at]),
                                           export_name(SORTED[insert_at - 1]));
       --insert_at) {
  }

  for (int i = u; i > insert_at; --i) {
    SORTED[i] = SORTED[i - 1];
  }

  SORTED[insert_at] = UNSORTED[u];
}

static void sort_exports_help() {
  SORTED[0] = UNSORTED[0];
  for (int u = 1; u < NUM_EXPORT; ++u) {
    sort_exports_one(u);
  }
}

static void copy_unsorted(uint16_t id) {
  for (uint16_t node = CHILD[id]; node != 0; node = SIBLING[node]) {
    UNSORTED[NUM_EXPORT] = node;
    ++NUM_EXPORT;
  }
}

static void copy_sorted(uint16_t id) {
  if (NUM_EXPORT == 0) {
    return;
  }
  CHILD[id] = SORTED[0];
  uint16_t node = CHILD[id];
  for (int i = 1; i < NUM_EXPORT; ++i) {
    SIBLING[node] = SORTED[i];
    node = SIBLING[node];
  }
  SIBLING[node] = 0;
}

static void sort_exports(uint16_t id) {
  if (SIBLING[CHILD[id]] == 0) {
    return;
  }
  NUM_EXPORT = 0;
  copy_unsorted(id);
  sort_exports_help();
  copy_sorted(id);
}

static int expose_all_variants_parse_help(uint16_t *id) {
  const int start = I;
  *id =
      module_expose_all_variants_node_init(); // node_init(MODULE_EXPOSE_ALL_VARIANTS_NODE);
  if (consume_upper_name_chars()) {
    return -1;
  }
  const int end = I;
  if (chunk_parse("(..)")) {
    return -1;
  }
  SRC_START[*id] = start + 1;
  SRC_SIZE[*id] = end - start;
  return 0;
}

static int expose_all_variants_parse(uint16_t *id) {
  const int start = I;
  const int result = expose_all_variants_parse_help(id);
  if (result) {
    I = start;
  }
  return result;
}

static int export_name_parse(uint16_t *id) {
  if (lower_name_parse(id) == 0) {
    return 0;
  }

  if (expose_all_variants_parse(id) == 0) {
    return 0;
  }

  return upper_name_parse(id);
}

static int export_parse(uint16_t *id) {
  const int comments_before = comments_parse();
  uint16_t name;
  const int name_result = export_name_parse(&name);
  if (name_result) {
    return name_result;
  }
  const int comments_after = comments_parse();

  *id = module_exports_all_node_init();
  CHILD[*id] = comments_before;
  SIBLING[comments_before] = name;
  SIBLING[name] = comments_after;
  return 0;
}

static int module_exports_explicit_parse_help(uint16_t *id) {
  *id = general_node_init();
  if (char_parse('(')) {
    return -1;
  }
  many_whitespace_parse();
  uint16_t export;
  if (export_parse(&export)) {
    return -1;
  }
  CHILD[*id] = export;
  uint16_t previous = export;
  while (1) {
    many_whitespace_parse();
    if (char_parse(',')) {
      break;
    }
    many_whitespace_parse();
    if (export_parse(&export)) {
      break;
    }
    SIBLING[previous] = export;
    previous = export;
  }
  many_whitespace_parse();
  if (char_parse(')')) {
    return -1;
  }
  sort_exports(*id);
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
    return -1;
  }
  many_whitespace_parse();
  if (chunk_parse("..")) {
    return -1;
  }
  many_whitespace_parse();
  if (char_parse(')')) {
    return -1;
  }
  *id = module_exports_all_node_init();
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
    return -1;
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

  *id = literal_node_init();
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

static int module_name_parse(uint16_t *id) {
  const int start = I;
  uint16_t dont_care;
  if (upper_name_parse(&dont_care)) {
    I = start;
    return -1;
  }

  while (1) {
    if (char_parse('.')) {
      break;
    }
    if (upper_name_parse(&dont_care)) {
      I = start;
      return -1;
    }
  }

  *id = literal_node_init();
  SRC_START[*id] = start + 1;
  SRC_SIZE[*id] = I - start;
  return 0;
}

static int not_hyphen_char_parse() {
  uint8_t c;
  const int result = any_char_parse(&c);
  if (result) {
    return result;
  }

  if (c == '-') {
    --I;
    return -1;
  }
  return 0;
}

static int doc_comment_parse(uint16_t *id) {
  const int start = I;
  if (chunk_parse("{-|")) {
    return -1;
  }
  while (not_hyphen_char_parse() == 0) {
  }
  if (chunk_parse("-}")) {
    I = start;
    return -1;
  }
  *id = general_node_init();
  SRC_START[*id] = start + 1;
  SRC_SIZE[*id] = I - start;

  return 0;
}

static int module_declaration_parse_help(uint16_t *id) {
  *id = ROOT;
  if (keyword_parse("module")) {
    return -1;
  }
  const uint16_t comment_before_name = comments_parse();
  uint16_t module_name;
  if (module_name_parse(&module_name)) {
    return -1;
  }
  const uint16_t comment_before_exposing = comments_parse();
  if (keyword_parse("exposing")) {
    return -1;
  }
  const uint16_t comment_after_exposing = comments_parse();
  uint16_t exports;
  const int module_exports_result = module_exports_parse(&exports);
  if (module_exports_result) {
    return module_exports_result;
  }
  CHILD[*id] = comment_before_name;
  SIBLING[comment_before_name] = module_name;
  SIBLING[module_name] = comment_before_exposing;
  SIBLING[comment_before_exposing] = comment_after_exposing;
  SIBLING[comment_after_exposing] = exports;
  const uint16_t normal_comment = comments_parse();
  SIBLING[exports] = normal_comment;
  many_whitespace_parse();
  uint16_t block;
  if (doc_comment_parse(&block) == 0) {
    SIBLING[normal_comment] = block;
    return 0;
  }
  SIBLING[normal_comment] = general_node_init();

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

static void export_left_comment_write(uint16_t id, int is_multiline) {
  const uint16_t left_comment = CHILD[id];
  comments_write(left_comment, 6);
  if (CHILD[left_comment] == 0) {
    return;
  }
  if (is_multiline) {
    fputs("\n      ", OUT);
    return;
  }
  fputc(' ', OUT);
}

static void export_name_write(uint16_t id) {
  const uint16_t name = SIBLING[CHILD[id]];
  literal_write(name);
  if (is_module_expose_all_variants_node(name)) {
    fputs("(..)", OUT);
  }
}

static void export_right_comment_write(uint16_t id, int is_multiline) {
  const uint16_t right_comment = SIBLING[SIBLING[CHILD[id]]];
  if (CHILD[right_comment] == 0) {
    return;
  }
  if (is_multiline) {
    fputs("\n      ", OUT);
  } else {
    fputc(' ', OUT);
  }
  comments_write(right_comment, 6);
}

static void export_write(uint16_t id, int is_multiline) {
  export_left_comment_write(id, is_multiline);
  export_name_write(id);
  export_right_comment_write(id, is_multiline);
}

static int module_export_has_multiline_comments(uint16_t id) {
  const uint16_t left_comment = CHILD[id];
  const uint16_t right_comment = SIBLING[SIBLING[left_comment]];
  return comments_are_multiline(left_comment) ||
         comments_are_multiline(right_comment);
}

static int is_multiline_module_exports(uint16_t id) {
  if (module_export_has_multiline_comments(CHILD[id])) {
    return 1;
  }
  for (uint16_t sibling = SIBLING[CHILD[id]]; sibling != 0;
       sibling = SIBLING[sibling]) {
    if (module_export_has_multiline_comments(sibling)) {
      return 1;
    }
  }
  return 0;
}

static void explicit_exports_write(uint16_t id, int is_multiline) {
  fputc('(', OUT);
  if (is_multiline) {
    fputc(' ', OUT);
  }
  const uint16_t export = CHILD[id];
  export_write(export, is_multiline);
  for (uint16_t sibling = SIBLING[export]; sibling != 0;
       sibling = SIBLING[sibling]) {
    if (is_multiline) {
      fputs("\n    ", OUT);
    }
    fputs(", ", OUT);
    export_write(sibling, is_multiline);
  }
  if (is_multiline) {
    fputs("\n    ", OUT);
  }
  fputc(')', OUT);
}

static void module_exports_write(uint16_t id, int is_multiline) {
  if (is_module_exports_all_node(id)) {
    fputs("(..)", OUT);
    return;
  }
  explicit_exports_write(id, is_multiline);
}

static void module_declaration_gap(int is_multiline) {
  if (is_multiline) {
    fputs("\n    ", OUT);
    return;
  }
  fputc(' ', OUT);
}

static void comment_with_gap_write(uint16_t id, int is_multiline) {
  module_declaration_gap(is_multiline);
  comments_write_module_declaration(id);
  if (CHILD[id] != 0) {
    module_declaration_gap(is_multiline);
  }
}

static void module_declaration_exports_with_gaps_write(uint16_t comment_before,
                                                       int is_multiline) {
  const uint16_t exports = SIBLING[comment_before];
  const int is_exports_multiline = is_multiline_module_exports(exports);
  comment_with_gap_write(comment_before, is_multiline || is_exports_multiline);
  module_exports_write(exports, is_exports_multiline);
  const uint16_t comment = SIBLING[exports];
  if (CHILD[comment] != 0) {
    fputs("\n\n", OUT);
  }
  comments_write(comment, 0);
  const uint16_t docs = SIBLING[comment];
  if (is_no_docs_node(docs)) {
    return;
  }
  fputs("\n\n", OUT);
  literal_write(docs);
}

static void module_declaration_write() {
  const uint16_t comment_before_name = CHILD[ROOT];
  const uint16_t name = SIBLING[comment_before_name];
  const uint16_t comment_after_name = SIBLING[name];
  const uint16_t comment_after_exposing = SIBLING[comment_after_name];
  const int is_multiline_declaration =
      comments_are_multiline(comment_before_name) ||
      comments_are_multiline(comment_after_exposing) ||
      comments_are_multiline(comment_after_name);
  fputs("module", OUT);
  comment_with_gap_write(comment_before_name,
                         comments_are_multiline(comment_before_name));
  literal_write(name);
  comment_with_gap_write(comment_after_name, is_multiline_declaration);
  fputs("exposing", OUT);
  module_declaration_exports_with_gaps_write(comment_after_exposing,
                                             is_multiline_declaration);
}

static int module_declaration_format() {
  const int parse_result = module_declaration_parse();
  if (parse_result) {
    return parse_result;
  }
  module_declaration_write();
  return 0;
}

static int with_out_file() {
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
