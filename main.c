#include <dirent.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>

static int expression_parse(int *node);
static int line_comment_parse(int *node);
static int comment_parse(int *node);
static void comments_parse(uint32_t comments[1000], int *num_comments);
static void left_comments_write(int node, int indent);
static int get_left_comment(int node, int *left_comment, int *i);
static int is_multiline_block_comment(int node);
static int is_empty_block_comment(int node);
static int is_line_comment(int node);
static void right_comments_write(int node, int indent);
static int is_multiline_comment(int node);
static int argument_parse(int *node);
static int triple_string_mask_any_char_parse(uint8_t *c, int *i);

// Twice the size of the largest Elm file I have seen.
#define MAX_SRC 1400 * 1000
static uint8_t SRC[MAX_SRC];
static uint8_t TRIPLE_STRING_MASK[MAX_SRC];
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

#define MAX_LEFT_COMMENT 10000
static uint32_t LEFT_COMMENT[MAX_LEFT_COMMENT];
static uint32_t LEFT_COMMENT_PARENT[MAX_LEFT_COMMENT];
int NUM_LEFT_COMMENT = 0;

#define MAX_EMPTY_BLOCK_COMMENT 10000
static uint32_t IS_EMPTY_BLOCK_COMMENT[MAX_EMPTY_BLOCK_COMMENT];
int NUM_EMPTY_BLOCK_COMMENT = 0;

#define MAX_BLOCK_COMMENT_LINE 10000
static uint32_t BLOCK_COMMENT_LINE_START[MAX_BLOCK_COMMENT_LINE];
static uint16_t BLOCK_COMMENT_LINE_SIZE[MAX_BLOCK_COMMENT_LINE];
static uint32_t BLOCK_COMMENT_LINE_PARENT[MAX_BLOCK_COMMENT_LINE];
int NUM_BLOCK_COMMENT_LINE = 0;

#define MAX_HAS_DOUBLE_HYPHEN_BLOCK 1000
static uint32_t HAS_DOUBLE_HYPHEN_BLOCK[MAX_HAS_DOUBLE_HYPHEN_BLOCK];
int NUM_HAS_DOUBLE_HYPHEN_BLOCK = 0;

#define MAX_EMPTY_LIST 10000
static uint32_t IS_EMPTY_LIST[MAX_EMPTY_LIST];
int NUM_EMPTY_LIST = 0;

#define MAX_LIST_ITEM 10000
static uint32_t LIST_ITEM[MAX_LIST_ITEM];
static uint32_t LIST[MAX_LIST_ITEM];
int NUM_LIST_ITEM = 0;

#define MAX_IS_MULTILINE 10000
static uint32_t IS_MULTILINE[MAX_IS_MULTILINE];
int NUM_IS_MULTILINE = 0;

#define MAX_SAME_LINE_COMMENT 10000
static uint32_t SAME_LINE_COMMENT[MAX_SAME_LINE_COMMENT];
static uint32_t SAME_LINE_COMMENT_PARENT[MAX_SAME_LINE_COMMENT];
int NUM_SAME_LINE_COMMENT = 0;

#define MAX_TITLE_COMMENT 10000
static uint32_t TITLE_COMMENT[MAX_TITLE_COMMENT];
static uint32_t TITLE_COMMENT_PARENT[MAX_TITLE_COMMENT];
int NUM_TITLE_COMMENT = 0;

#define MAX_DOUBLE_HYPHEN_BLOCK 10000
static uint32_t DOUBLE_HYPHEN_BLOCK[MAX_DOUBLE_HYPHEN_BLOCK];
int NUM_DOUBLE_HYPHEN_BLOCK = 0;

#define MAX_ARGUMENT 10000
static uint32_t ARGUMENT[MAX_ARGUMENT];
static uint32_t ARGUMENT_PARENT[MAX_ARGUMENT];
int NUM_ARGUMENT = 0;

#define MAX_IS_SINGLE_LINE 10000
static uint32_t IS_SINGLE_LINE[MAX_IS_SINGLE_LINE];
int NUM_IS_SINGLE_LINE = 0;

static void append_is_single_line(int node) {
  if (NUM_IS_SINGLE_LINE == MAX_IS_SINGLE_LINE) {
    fprintf(stderr, "too many single line nodes, maximum is %d\n",
            MAX_IS_SINGLE_LINE);
    exit(-1);
  }
  IS_SINGLE_LINE[NUM_IS_SINGLE_LINE] = node;
  ++NUM_IS_SINGLE_LINE;
}

static void append_argument(int parent, int child) {
  if (NUM_ARGUMENT == MAX_ARGUMENT) {
    fprintf(stderr, "too many arguments, maximum is %d\n", MAX_ARGUMENT);
    exit(-1);
  }
  ARGUMENT[NUM_ARGUMENT] = child;
  ARGUMENT_PARENT[NUM_ARGUMENT] = parent;
  ++NUM_ARGUMENT;
}

static void append_is_double_hyphen_block_comment(int node) {
  if (NUM_DOUBLE_HYPHEN_BLOCK == MAX_DOUBLE_HYPHEN_BLOCK) {
    fprintf(stderr,
            "too many double hyphen block comment nodes, maximum is %d\n",
            MAX_DOUBLE_HYPHEN_BLOCK);
    exit(-1);
  }
  DOUBLE_HYPHEN_BLOCK[NUM_DOUBLE_HYPHEN_BLOCK] = node;
  ++NUM_DOUBLE_HYPHEN_BLOCK;
}

static int is_any_multiline_title_comment(int node) {
  for (int i = 0; i < NUM_TITLE_COMMENT; ++i) {
    if (TITLE_COMMENT_PARENT[i] == (uint32_t)node &&
        is_multiline_comment(TITLE_COMMENT[i])) {
      return 1;
    }
  }
  return 0;
}

static int is_multiline(int node) {
  for (int i = 0; i < NUM_IS_MULTILINE; ++i) {
    if (IS_MULTILINE[i] == (uint32_t)node) {
      return 1;
    }
  }
  return 0;
}

static void append_same_line_comment(int node, int comment) {
  if (NUM_SAME_LINE_COMMENT == MAX_SAME_LINE_COMMENT) {
    fprintf(stderr, "too many same line comment nodes, maximum is %d\n",
            MAX_SAME_LINE_COMMENT);
    exit(-1);
  }
  SAME_LINE_COMMENT[NUM_SAME_LINE_COMMENT] = comment;
  SAME_LINE_COMMENT_PARENT[NUM_SAME_LINE_COMMENT] = node;
  ++NUM_SAME_LINE_COMMENT;
}

static void append_title_comment(int node, int comment) {
  if (NUM_TITLE_COMMENT == MAX_TITLE_COMMENT) {
    fprintf(stderr, "too many title comment nodes, maximum is %d\n",
            MAX_TITLE_COMMENT);
    exit(-1);
  }
  TITLE_COMMENT[NUM_TITLE_COMMENT] = comment;
  TITLE_COMMENT_PARENT[NUM_TITLE_COMMENT] = node;
  ++NUM_TITLE_COMMENT;
}

static void append_is_multiline(int node) {
  if (NUM_IS_MULTILINE == MAX_IS_MULTILINE) {
    fprintf(stderr, "too many nodes, maximum is %d\n", MAX_IS_MULTILINE);
    exit(-1);
  }
  IS_MULTILINE[NUM_IS_MULTILINE] = node;
  ++NUM_IS_MULTILINE;
}

static void append_list_item(int parent, int child) {
  if (NUM_LIST_ITEM == MAX_LIST_ITEM) {
    fprintf(stderr, "too many list items, maximum is %d\n", MAX_LIST_ITEM);
    exit(-1);
  }
  LIST_ITEM[NUM_LIST_ITEM] = child;
  LIST[NUM_LIST_ITEM] = parent;
  ++NUM_LIST_ITEM;
}

static void append_is_empty_list(int node) {
  if (NUM_EMPTY_LIST == MAX_EMPTY_LIST) {
    fprintf(stderr, "too many empty list nodes, maximum is %d\n",
            MAX_EMPTY_LIST);
    exit(-1);
  }
  IS_EMPTY_LIST[NUM_EMPTY_LIST] = node;
  ++NUM_EMPTY_LIST;
}

static void append_double_hyphen_start_block(int node) {
  if (NUM_HAS_DOUBLE_HYPHEN_BLOCK == MAX_HAS_DOUBLE_HYPHEN_BLOCK) {
    fprintf(stderr, "too many double hyphen start block nodes, maximum is %d\n",
            MAX_HAS_DOUBLE_HYPHEN_BLOCK);
    exit(-1);
  }
  HAS_DOUBLE_HYPHEN_BLOCK[NUM_HAS_DOUBLE_HYPHEN_BLOCK] = node;
  ++NUM_HAS_DOUBLE_HYPHEN_BLOCK;
}

static void append_block_comment_line(int parent, int start, int size) {
  if (NUM_BLOCK_COMMENT_LINE == MAX_BLOCK_COMMENT_LINE) {
    fprintf(stderr, "too many block comment lines, maximum is %d\n",
            MAX_BLOCK_COMMENT_LINE);
    exit(-1);
  }
  BLOCK_COMMENT_LINE_START[NUM_BLOCK_COMMENT_LINE] = start;
  BLOCK_COMMENT_LINE_SIZE[NUM_BLOCK_COMMENT_LINE] = size;
  BLOCK_COMMENT_LINE_PARENT[NUM_BLOCK_COMMENT_LINE] = parent;
  ++NUM_BLOCK_COMMENT_LINE;
}

static void append_is_empty_block_comment(int node) {
  if (NUM_EMPTY_BLOCK_COMMENT == MAX_EMPTY_BLOCK_COMMENT) {
    fprintf(stderr, "too many empty block comment nodes, maximum is %d\n",
            MAX_EMPTY_BLOCK_COMMENT);
    exit(-1);
  }
  IS_EMPTY_BLOCK_COMMENT[NUM_EMPTY_BLOCK_COMMENT] = node;
  ++NUM_EMPTY_BLOCK_COMMENT;
}

static void append_left_comment(int node, int comment_node) {
  if (NUM_LEFT_COMMENT == MAX_LEFT_COMMENT) {
    fprintf(stderr, "too many nodes, maximum is %d\n", MAX_LEFT_COMMENT);
    exit(-1);
  }
  LEFT_COMMENT[NUM_LEFT_COMMENT] = comment_node;
  LEFT_COMMENT_PARENT[NUM_LEFT_COMMENT] = node;
  ++NUM_LEFT_COMMENT;
}

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

static int is_empty_list(int node) {
  for (int i = 0; i < NUM_EMPTY_LIST; ++i) {
    if (IS_EMPTY_LIST[i] == (uint32_t)node) {
      return 1;
    }
  }
  return 0;
}

static void expression_write(int node, int indent);

static int get_list_item(int node, int *item, int *start) {
  for (int i = *start; i < NUM_LIST_ITEM; ++i) {
    if (LIST[i] == (uint32_t)node) {
      *item = LIST_ITEM[i];
      *start = i + 1;
      return 0;
    }
  }
  return -1;
}

static int has_left_comment(int node) {
  for (int i = 0; i < NUM_LEFT_COMMENT; ++i) {
    if (LEFT_COMMENT_PARENT[i] == (uint32_t)node) {
      return 1;
    }
  }
  return 0;
}

static int is_multiline_comment(int node) {
  return is_empty_block_comment(node) || is_multiline_block_comment(node) ||
         is_line_comment(node);
}

static int has_multiline_left_comment(int node) {
  int left_comment;
  int start = 0;
  while (get_left_comment(node, &left_comment, &start) == 0) {
    if (is_multiline_comment(left_comment)) {
      return 1;
    }
  }
  return 0;
}

static int get_src(int node, int *src_index) {
  for (int i = 0; i < NUM_HAS_SRC; ++i) {
    if (HAS_SRC[i] == (uint32_t)node) {
      *src_index = i;
      return 1;
    }
  }
  return 0;
}

static int is_line_comment(int node) {
  int src_index;
  if (get_src(node, &src_index) == 0) {
    return 0;
  }
  const int start = SRC_START[src_index];
  const int size = SRC_SIZE[src_index];
  if (size < 2) {
    return 0;
  }
  return SRC[start] == '-' && SRC[start + 1] == '-';
}

void indent_write(int indent) {
  fputc('\n', OUT);
  for (int i = 0; i < indent; ++i) {
    fputc(' ', OUT);
  }
}

static int has_same_line_comment(int node) {
  for (int i = 0; i < NUM_SAME_LINE_COMMENT; ++i) {
    if (SAME_LINE_COMMENT_PARENT[i] == (uint32_t)node) {
      return 1;
    }
  }
  return 0;
}

static void list_item_write(int item, int indent) {
  const int left_is_multiline = has_multiline_left_comment(item);
  left_comments_write(item, indent + 2);
  if (left_is_multiline) {
    indent_write(indent + 2);
  }
  if (has_left_comment(item) && !left_is_multiline) {
    fputc(' ', OUT);
  }
  expression_write(item, indent + 2);
  if (has_same_line_comment(item)) {
    fputc(' ', OUT);
  }
  right_comments_write(item, indent);
}

static void non_empty_list_write(int node, int indent) {
  fputs("[ ", OUT);
  int item;
  int start = 0;
  if (get_list_item(node, &item, &start) == 0) {
    list_item_write(item, indent);
  }
  int left_is_multiline = has_multiline_left_comment(item);
  while (get_list_item(node, &item, &start) == 0) {
    left_is_multiline = has_multiline_left_comment(item);
    if (left_is_multiline || is_multiline(node)) {
      indent_write(indent);
    }
    fputs(", ", OUT);
    list_item_write(item, indent);
  }
  if (left_is_multiline || is_multiline(node)) {
    indent_write(indent);
  } else {
    fputc(' ', OUT);
  }
  fputc(']', OUT);
}

static int is_non_empty_list(int node) {
  for (int i = 0; i < NUM_LIST_ITEM; ++i) {
    if (LIST[i] == (uint32_t)node) {
      return 1;
    }
  }
  return 0;
}

static int get_argument(int node, int *argument, int *start) {
  for (int i = *start; i < NUM_ARGUMENT; ++i) {
    if (ARGUMENT_PARENT[i] == (uint32_t)node) {
      *argument = ARGUMENT[i];
      *start = i + 1;
      return 0;
    }
  }
  return -1;
}

int floor_to_four(int x) { return x / 4 * 4; }

static int is_single_line(int node) {
  for (int i = 0; i < NUM_IS_SINGLE_LINE; ++i) {
    if (IS_SINGLE_LINE[i] == (uint32_t)node) {
      return 1;
    }
  }
  return 0;
}

static void argument_write(int is_multi, int argument, int indent) {
  if (is_multi && !is_single_line(argument)) {
    indent_write(floor_to_four(indent + 4));
  } else {
    fputc(' ', OUT);
  }
  left_comments_write(argument, floor_to_four(indent + 4));
  int left_is_multiline = has_multiline_left_comment(argument);
  int has_left = has_left_comment(argument);
  if (has_left && !left_is_multiline) {
    fputc(' ', OUT);
  }
  if (has_left && left_is_multiline) {
    indent_write(floor_to_four(indent + 4));
  }
  expression_write(argument, indent + 4);
}

static void function_call_write(int node, int indent) {
  src_write(node);
  int argument;
  int start = 0;
  get_argument(node, &argument, &start);
  const int is_multi = is_multiline(node);
  argument_write(is_multi, argument, indent);
  while (get_argument(node, &argument, &start) == 0) {
    argument_write(is_multi, argument, indent);
  }
}

static int has_arguments(int node) {
  for (int i = 0; i < NUM_ARGUMENT; ++i) {
    if (ARGUMENT_PARENT[i] == (uint32_t)node) {
      return 1;
    }
  }
  return 0;
}

static void expression_write(int node, int indent) {
  if (is_hex(node)) {
    hex_write(node);
    return;
  }
  if (is_exponent_float(node)) {
    exponent_float_write(node);
    return;
  }
  if (is_empty_list(node)) {
    fputs("[]", OUT);
    return;
  }
  if (is_non_empty_list(node)) {
    non_empty_list_write(node, indent);
    return;
  }
  if (has_arguments(node)) {
    function_call_write(node, indent);
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
  NUM_LEFT_COMMENT = 0;
  NUM_BLOCK_COMMENT_LINE = 0;
  NUM_EMPTY_BLOCK_COMMENT = 0;
  NUM_HAS_DOUBLE_HYPHEN_BLOCK = 0;
  NUM_EMPTY_LIST = 0;
  NUM_LIST_ITEM = 0;
  NUM_IS_MULTILINE = 0;
  NUM_SAME_LINE_COMMENT = 0;
  NUM_TITLE_COMMENT = 0;
  NUM_DOUBLE_HYPHEN_BLOCK = 0;
  NUM_ARGUMENT = 0;
  NUM_IS_SINGLE_LINE = 0;
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
  if (chunk_parse("\\n") == 0) {
    return 0;
  }
  if (chunk_parse("\\t") == 0) {
    return 0;
  }
  return unicode_hex_parse();
}

static int triple_string_item_parse() {
  const int start = I;
  if (chunk_parse("\"\"\"") == 0) {
    I = start;
    return -1;
  }
  if (chunk_parse("\\\"") == 0) {
    return 0;
  }
  if (chunk_parse("\\\\") == 0) {
    return 0;
  }
  if (unicode_hex_parse() == 0) {
    return 0;
  }
  uint8_t dont_care;
  if (any_char_parse(&dont_care)) {
    I = start;
    return -1;
  }
  return 0;
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
  if (chunk_parse("\"\"\"")) {
    return -1;
  }
  while (triple_string_item_parse() == 0) {
  }
  if (chunk_parse("\"\"\"")) {
    I = start;
    return -1;
  }
  *node = get_new_node();
  append_has_src(*node, start + 1, I - start);
  return 0;
}

static int first_lower_name_char_parse() {
  uint8_t c;
  if (any_char_parse(&c)) {
    return -1;
  }
  if (c >= 'a' && c <= 'z') {
    return 0;
  }
  --I;
  return -1;
}

static int first_upper_name_char_parse() {
  uint8_t c;
  if (any_char_parse(&c)) {
    return -1;
  }
  if (c >= 'A' && c <= 'Z') {
    return 0;
  }
  --I;
  return -1;
}

static int subsequent_name_char_parse() {
  uint8_t c;
  if (any_char_parse(&c)) {
    return -1;
  }
  if ((c >= 'a' && c <= 'z') || c == '_' || (c >= '0' && c <= '9') ||
      (c >= 'A' && c <= 'Z')) {
    return 0;
  }
  --I;
  return -1;
}

static int lower_name_parse(int *node) {
  const int start = I;
  if (first_lower_name_char_parse()) {
    return -1;
  }

  while (subsequent_name_char_parse() == 0) {
  }

  *node = get_new_node();
  append_has_src(*node, start + 1, I - start);
  return 0;
}

static int upper_name_parse(int *node) {
  const int start = I;
  if (first_upper_name_char_parse()) {
    return -1;
  }

  while (subsequent_name_char_parse() == 0) {
  }

  *node = get_new_node();
  append_has_src(*node, start + 1, I - start);
  return 0;
}

static int empty_list_parse(int *node) {
  const int start = I;
  if (char_parse('[')) {
    return -1;
  }
  while (char_parse(' ') == 0 || char_parse('\n') == 0) {
  }
  if (char_parse(']')) {
    I = start;
    return -1;
  }
  *node = get_new_node();
  append_is_empty_list(*node);
  return 0;
}

static void list_item_after_expression_parse(int node) {
  while (char_parse(' ') == 0) {
  }
  int same_line_comment;
  if (line_comment_parse(&same_line_comment) == 0) {
    append_same_line_comment(node, same_line_comment);
  }
  while (char_parse(' ') == 0 || char_parse('\n') == 0) {
  }
  static uint32_t title_comment[1000];
  int num_title_comment = 0;
  comments_parse(title_comment, &num_title_comment);
  while (char_parse(' ') == 0 || char_parse('\n') == 0) {
  }
  for (int i = 0; i < num_title_comment; ++i) {
    append_title_comment(node, title_comment[i]);
  }
}

static int list_item_parse(int *node) {
  const int start = I;
  while (char_parse(' ') == 0 || char_parse('\n') == 0) {
  }
  static uint32_t left_comment[1000];
  int num_left_comment = 0;
  comments_parse(left_comment, &num_left_comment);
  while (char_parse(' ') == 0 || char_parse('\n') == 0) {
  }
  if (expression_parse(node)) {
    I = start;
    return -1;
  }
  for (int i = 0; i < num_left_comment; ++i) {
    append_left_comment(*node, left_comment[i]);
  }
  list_item_after_expression_parse(*node);
  return 0;
}

static int non_empty_list_parse(int *node) {
  const int start = I;
  const int start_row = ROW[I];
  if (char_parse('[')) {
    return -1;
  }
  int item;
  if (list_item_parse(&item)) {
    I = start;
    return -1;
  }
  *node = get_new_node();
  append_list_item(*node, item);
  while (1) {
    if (char_parse(',')) {
      break;
    }
    if (list_item_parse(&item)) {
      I = start;
      return -1;
    }
    append_list_item(*node, item);
  }
  if (char_parse(']')) {
    I = start;
    return -1;
  }
  const int is_multiline = ROW[I] - start_row;
  if (is_multiline) {
    append_is_multiline(*node);
  }
  return 0;
}

static int list_parse(int *node) {
  if (empty_list_parse(node) == 0) {
    return 0;
  }

  return non_empty_list_parse(node);
}

static int argument_and_comments_parse(int *argument) {
  while (char_parse(' ') == 0 || char_parse('\n') == 0) {
  }
  static uint32_t left_comments[1000];
  int num_left_comments = 0;
  comments_parse(left_comments, &num_left_comments);
  if (argument_parse(argument)) {
    return -1;
  }
  for (int i = 0; i < num_left_comments; ++i) {
    append_left_comment(*argument, left_comments[i]);
  }
  return 0;
}

static int function_call_parse(int *node) {
  const int start = I;
  const int start_row = ROW[I];
  if (lower_name_parse(node)) {
    return -1;
  }
  int argument;
  const int first_arg_result = argument_and_comments_parse(&argument);
  if (first_arg_result) {
    I = start;
    return -1;
  }
  append_argument(*node, argument);
  if (ROW[I] == start_row) {
    append_is_single_line(argument);
  }
  while (1) {
    if (argument_and_comments_parse(&argument)) {
      break;
    }
    append_argument(*node, argument);
  }
  if (ROW[I] > start_row) {
    append_is_multiline(*node);
  }
  return 0;
}

static int argument_parse(int *node) {
  if (lower_name_parse(node) == 0) {
    return 0;
  }
  if (float_parse(node) == 0) {
    return 0;
  }
  if (triple_string_parse(node) == 0) {
    return 0;
  }
  if (list_parse(node) == 0) {
    return 0;
  }
  return int_parse(node);
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
  if (normal_string_parse(node) == 0) {
    return 0;
  }
  if (function_call_parse(node) == 0) {
    return 0;
  }
  if (upper_name_parse(node) == 0) {
    return 0;
  }
  if (lower_name_parse(node) == 0) {
    return 0;
  }
  return list_parse(node);
}

static int not_newline_parse() {
  uint8_t c;
  if (any_char_parse(&c)) {
    return -1;
  }
  if (c == '\n') {
    --I;
    return -1;
  }
  return 0;
}

static int line_comment_parse(int *node) {
  const int start = I;
  if (chunk_parse("--")) {
    return -1;
  }
  while (not_newline_parse() == 0) {
  }
  while (SRC[I] == ' ') {
    --I;
  }
  const int end = I;
  while (char_parse(' ') == 0) {
  }
  *node = get_new_node();
  append_has_src(*node, start + 1, end - start);
  return 0;
}

static int empty_block_comment_parse(int *node) {
  const int start = I;
  *node = get_new_node();
  if (chunk_parse("{-")) {
    return -1;
  }
  while (char_parse(' ') == 0) {
  }
  if (chunk_parse("-}")) {
    I = start;
    return -1;
  }
  append_is_empty_block_comment(*node);
  return 0;
}

static int block_comment_item_parse(int *nesting) {
  if (*nesting == 1 && chunk_parse("-}") == 0) {
    I = I - 2;
    return -1;
  }
  if (chunk_parse("-}") == 0) {
    --*nesting;
    return 0;
  }
  if (chunk_parse("{-") == 0) {
    ++*nesting;
    return 0;
  }
  if (char_parse('\n') == 0) {
    --I;
    return -1;
  }
  uint8_t c;
  return any_char_parse(&c);
}

static void block_comment_line_parse(uint32_t *start, uint16_t *size,
                                     int *nesting) {
  while (char_parse(' ') == 0) {
  }
  *start = I + 1;
  while (block_comment_item_parse(nesting) == 0) {
  }
  while (SRC[I] == ' ') {
    --I;
  }
  *size = I - *start + 1;
  while (char_parse(' ') == 0) {
  }
  char_parse('\n');
}

static int is_block_comment_end() {
  const int start = I;
  while (char_parse(' ') == 0) {
  }
  if (chunk_parse("-}")) {
    I = start;
    return 0;
  }
  I = start;
  return 1;
}

static int non_empty_block_comment_parse(int *node) {
  const int start = I;
  if (chunk_parse("{-")) {
    return -1;
  }
  *node = get_new_node();
  if (char_parse('-') == 0) {
    --I;
    append_double_hyphen_start_block(*node);
  }
  uint32_t line_start;
  uint16_t line_size;
  int nesting = 1;
  while (1) {
    if (nesting == 1 && is_block_comment_end()) {
      break;
    }
    block_comment_line_parse(&line_start, &line_size, &nesting);
    append_block_comment_line(*node, line_start, line_size);
  }
  while (char_parse(' ') == 0) {
  }
  if (chunk_parse("-}")) {
    I = start;
    return -1;
  }
  return 0;
}

static int double_hyphen_block_comment_parse(int *node) {
  if (chunk_parse("{--")) {
    return -1;
  }
  while (char_parse(' ') == 0) {
  }
  const int start = I;
  uint8_t dont_care;
  while (chunk_parse("-}") && any_char_parse(&dont_care) == 0) {
  }
  I -= 2;
  while (SRC[I] == ' ') {
    --I;
  }
  const int end = I;
  while (char_parse(' ') == 0) {
  }
  if (chunk_parse("-}")) {
    I = start;
    return -1;
  }
  *node = get_new_node();
  append_has_src(*node, start + 1, end - start);
  append_is_double_hyphen_block_comment(*node);
  return 0;
}

static int comment_parse(int *node) {
  if (line_comment_parse(node) == 0) {
    return 0;
  }
  if (empty_block_comment_parse(node) == 0) {
    return 0;
  }
  if (double_hyphen_block_comment_parse(node) == 0) {
    return 0;
  }
  return non_empty_block_comment_parse(node);
}

static void comments_parse(uint32_t comments[1000], int *num_comments) {
  for (; *num_comments < 1000 &&
         comment_parse((int *)&comments[*num_comments]) == 0;
       ++*num_comments) {
    while (char_parse(' ') == 0 || char_parse('\n') == 0) {
    }
  }
}

static int module_parse(int *node) {
  if (chunk_parse("module X exposing (x)\n\n\nx =\n    ")) {
    return -1;
  }
  static uint32_t left_comment[1000];
  int num_left_comment = 0;
  comments_parse(left_comment, &num_left_comment);
  while (char_parse(' ') == 0) {
  }
  if (expression_parse(node)) {
    return -1;
  }
  for (int i = 0; i < num_left_comment; ++i) {
    append_left_comment(*node, left_comment[i]);
  }
  return 0;
}

static int get_left_comment(int node, int *left_comment, int *i) {
  for (; *i < NUM_LEFT_COMMENT; ++*i) {
    if (LEFT_COMMENT_PARENT[*i] == (uint32_t)node) {
      *left_comment = LEFT_COMMENT[*i];
      ++*i;
      return 0;
    }
  }
  return -1;
}

static int get_title_comment(int node, int *comment, int *i) {
  for (; *i < NUM_TITLE_COMMENT; ++*i) {
    if (TITLE_COMMENT_PARENT[*i] == (uint32_t)node) {
      *comment = TITLE_COMMENT[*i];
      ++*i;
      return 0;
    }
  }
  return -1;
}

static int is_non_empty_block_comment(int node) {
  for (int i = 0; i < NUM_BLOCK_COMMENT_LINE; ++i) {
    if (BLOCK_COMMENT_LINE_PARENT[i] == (uint32_t)node) {
      return 1;
    }
  }
  return 0;
}

static int is_double_hyphen_block_comment(int node) {
  for (int i = 0; i < NUM_DOUBLE_HYPHEN_BLOCK; ++i) {
    if (DOUBLE_HYPHEN_BLOCK[i] == (uint32_t)node) {
      return 1;
    }
  }
  return 0;
}

static int is_empty_block_comment(int node) {
  for (int i = 0; i < NUM_EMPTY_BLOCK_COMMENT; ++i) {
    if (IS_EMPTY_BLOCK_COMMENT[i] == (uint32_t)node) {
      return 1;
    }
  }
  return 0;
}

static int is_multiline_block_comment(int node) {
  int num_lines = 0;
  for (int i = 0; i < NUM_BLOCK_COMMENT_LINE && num_lines < 3; ++i) {
    if (BLOCK_COMMENT_LINE_PARENT[i] == (uint32_t)node) {
      ++num_lines;
    }
  }
  return num_lines > 1;
}

static int get_comment_line(int node, int *line, int start) {
  for (int i = start; i < NUM_BLOCK_COMMENT_LINE; ++i) {
    if (BLOCK_COMMENT_LINE_PARENT[i] == (uint32_t)node) {
      *line = i;
      return 0;
    }
  }
  return -1;
}

static void block_comment_line_write(int line, int i, int indent) {
  const uint32_t start = BLOCK_COMMENT_LINE_START[line];
  const uint16_t size = BLOCK_COMMENT_LINE_SIZE[line];
  if (i == 0 && size > 0) {
    fputc(' ', OUT);
  }
  if (i > 0) {
    fputc('\n', OUT);
  }
  if (size > 0 && i > 0) {
    for (int j = 0; j < indent; ++j) {
      fputc(' ', OUT);
    }
    fputs("   ", OUT);
  }
  fwrite(SRC + start, 1, size, OUT);
}

static void non_empty_block_comment_write(int node, int indent) {
  fputs("{-", OUT);
  const int is_multi = is_multiline_block_comment(node);
  int start = 0;
  int i = 0;
  for (int line; get_comment_line(node, &line, start) == 0;
       start = line + 1, ++i) {
    block_comment_line_write(line, i, indent);
  }
  if (is_multi) {
    indent_write(indent);
  }
  if (!is_multi) {
    fputc(' ', OUT);
  }
  fputs("-}", OUT);
}

static void double_hypen_block_comment_write(int node) {
  fputs("{--", OUT);
  src_write(node);
  fputs("-}", OUT);
}

static void comment_write(int node, int indent) {
  if (is_empty_block_comment(node)) {
    fputs("{--}", OUT);
    return;
  }
  if (is_non_empty_block_comment(node)) {
    non_empty_block_comment_write(node, indent);
    return;
  }
  if (is_double_hyphen_block_comment(node)) {
    double_hypen_block_comment_write(node);
    return;
  }
  src_write(node);
}

static void left_comments_write(int node, int indent) {
  int left_comment;
  int start = 0;
  if (get_left_comment(node, &left_comment, &start) == 0) {
    comment_write(left_comment, indent);
  }
  while (get_left_comment(node, &left_comment, &start) == 0) {
    indent_write(indent);
    comment_write(left_comment, indent);
  }
}

static int get_same_line_comment(int node, int *same_line_comment) {
  for (int i = 0; i < NUM_SAME_LINE_COMMENT; ++i) {
    if (SAME_LINE_COMMENT_PARENT[i] == (uint32_t)node) {
      *same_line_comment = SAME_LINE_COMMENT[i];
      return 0;
    }
  }
  return -1;
}

static void same_line_comment_write(int node, int indent) {
  int same_line_comment;
  if (get_same_line_comment(node, &same_line_comment) == 0) {
    comment_write(same_line_comment, indent);
  }
}

static void title_comments_write(int node, int indent) {
  int title_comment;
  int start = 0;
  if (get_title_comment(node, &title_comment, &start) == 0) {
    indent_write(indent);
    comment_write(title_comment, indent);
  }
  const int is_multi = is_any_multiline_title_comment(node);
  while (get_title_comment(node, &title_comment, &start) == 0) {
    if (is_multi) {
      indent_write(indent);
    } else {
      fputc(' ', OUT);
    }
    comment_write(title_comment, indent);
  }
}

static int has_title_comment(int node) {
  for (int i = 0; i < NUM_TITLE_COMMENT; ++i) {
    if (TITLE_COMMENT_PARENT[i] == (uint32_t)node) {
      return 1;
    }
  }
  return 0;
}

static void right_comments_write(int node, int indent) {
  same_line_comment_write(node, indent);
  if (has_title_comment(node)) {
    fputc('\n', OUT);
  }
  title_comments_write(node, indent);
}

static void module_write(int node) {
  fputs("module X exposing (x)\n\n\nx =\n    ", OUT);
  left_comments_write(node, 4);
  if (has_left_comment(node)) {
    fputs("\n    ", OUT);
  }
  expression_write(node, 4);
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
    if (SRC[i] == '\n' && !TRIPLE_STRING_MASK[i]) {
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

static int triple_string_mask_any_char_parse(uint8_t *c, int *i) {
  if (*i == NUM_SRC - 1) {
    return -1;
  }
  ++*i;
  *c = SRC[*i];
  return 0;
}

static int triple_string_mask_char_parse(uint8_t c, int *i) {
  if (*i == NUM_SRC - 1) {
    return -1;
  }
  ++*i;
  if (SRC[*i] != c) {
    --*i;
    return -1;
  }
  return 0;
}

static int triple_string_mask_chunk_parse(char *chunk, int *i) {
  const int start = *i;
  for (; *chunk != '\0' && triple_string_mask_char_parse(*chunk, i) == 0;
       ++chunk) {
  }
  if (*chunk != '\0') {
    *i = start;
    return -1;
  }
  return 0;
}

static int triple_string_mask_triple_string_item_parse(int *i) {
  const int start = *i;
  if (triple_string_mask_chunk_parse("\"\"\"", i) == 0) {
    *i = start;
    return -1;
  }
  if (triple_string_mask_chunk_parse("\\\"", i) == 0) {
    return 0;
  }
  if (triple_string_mask_chunk_parse("\\\\", i) == 0) {
    return 0;
  }
  if (triple_string_mask_chunk_parse("\\u", i) == 0) {
    return 0;
  }
  uint8_t dont_care;
  if (triple_string_mask_any_char_parse(&dont_care, i)) {
    *i = start;
    return -1;
  }
  return 0;
}

static int triple_string_mask_triple_string_parse(int *i) {
  const int start = *i;
  if (triple_string_mask_chunk_parse("\"\"\"", i)) {
    return -1;
  }
  while (triple_string_mask_triple_string_item_parse(i) == 0) {
  }
  if (triple_string_mask_chunk_parse("\"\"\"", i)) {
    return -1;
  }
  const int end = *i;
  for (int j = start; j < end; ++j) {
    TRIPLE_STRING_MASK[j] = 1;
  }
  return 0;
}

static int triple_string_mask_not_newline_parse(int *i) {
  uint8_t c;
  if (triple_string_mask_any_char_parse(&c, i)) {
    return -1;
  }
  if (c == '\n') {
    --*i;
    return -1;
  }
  return 0;
}

static int triple_string_mask_line_comment_parse(int *i) {
  if (triple_string_mask_chunk_parse("--", i)) {
    return -1;
  }
  while (triple_string_mask_not_newline_parse(i) == 0) {
  }
  return 0;
}

static int triple_string_mask_block_comment_item_parse(int *i, int *nesting) {
  if (*nesting == 1 && triple_string_mask_chunk_parse("-}", i) == 0) {
    *i = *i - 2;
    return -1;
  }
  if (triple_string_mask_chunk_parse("-}", i) == 0) {
    --*nesting;
    return 0;
  }
  if (triple_string_mask_chunk_parse("{-", i) == 0) {
    ++*nesting;
    return 0;
  }
  uint8_t dont_care;
  if (triple_string_mask_any_char_parse(&dont_care, i)) {
    return -1;
  }
  return 0;
}

static int triple_string_mask_block_comment_parse(int *i) {
  if (triple_string_mask_chunk_parse("{-", i)) {
    return -1;
  }
  int nesting = 1;
  while (triple_string_mask_block_comment_item_parse(i, &nesting) == 0) {
  }
  if (triple_string_mask_chunk_parse("-}", i)) {
    return -1;
  }
  return 0;
}

static int calculate_triple_string_mask_item(int *i) {
  if (triple_string_mask_triple_string_parse(i) == 0) {
    return 0;
  }
  if (triple_string_mask_line_comment_parse(i) == 0) {
    return 0;
  }
  if (triple_string_mask_block_comment_parse(i) == 0) {
    return 0;
  }
  uint8_t dont_care;
  return triple_string_mask_any_char_parse(&dont_care, i);
}

static void calculate_triple_string_mask() {
  for (int i = 0; i < NUM_SRC; ++i) {
    TRIPLE_STRING_MASK[i] = 0;
  }
  int i = 0;
  while (calculate_triple_string_mask_item(&i) == 0) {
  }
}

static void format_file(char *path) {
  if (read_src(path)) {
    return;
  }

  calculate_triple_string_mask();
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
