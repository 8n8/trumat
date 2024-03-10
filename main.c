#include <dirent.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

static int get_if_then_else(int node, int *condition, int *then_branch,
                            int *else_branch);
static void if_then_else_write(int is_in_else, int condition, int then_branch,
                               int else_branch, int indent);
static int dot_function_parse(int *node);
static int is_double_hyphen_block_comment(int node);
static int get_argument(int node, int *argument, int *start);
static int get_list_item(int node, int *item, int *start);
static void comment_write(int node, int indent);
static int is_single_line_left_comments(int node);
static int expression_parse(int *node);
static void left_comments_after_then_write(int node, int indent);
static int get_in_parens(int node, int *expression);
static int qualified_name_parse(int *node);
static void right_comments_with_spaces_write(int node, int indent);
static int get_tuple_item(int node, int *item, int *start);
static int backwards_char_parse(uint8_t c, int *i);
static int is_if_then_else_node(int node);
static void left_comments_with_spaces_write(int is_multi_context, int node,
                                            int indent);
static int if_then_else_parse(int *node);
static int keyword_parse(char *keyword);
static int record_parse(int *node);
static int infixed_parse(int *node);
static int argument_in_unnecessary_parens_parse(int *node);
static int has_right_comment(int node);
static int simple_expression_parse(int *node);
static int get_dot_function(int node, int *dot_function);
static int infixed_item_parse(int *node);
static int floor_to_four(int x);
static int line_comment_parse(int *node);
static int in_unnecessary_parens_parse(int *node);
static int comment_parse(int *node);
static int has_title_comment(int node);
static int string_hex_parse(int *node);
static int string_length(char *s);
static void attach_title_comments(int node, int title_comments);
static void right_comments_in_expression_write(int node, int indent);
static void title_comments_parse(int parent);
static int get_normal_string_item(int node, int *item, int *start);
static int is_single_line_right_comments(int node);
static int get_right_comment(int node, int *right_comment, int *i);
static void attach_left_comment(int node, int left_comment);
static int get_same_line_comment(int node, int *same_line_comment);
static int right_comments_in_expression_parse();
static void attach_right_comment(int node, int right_comment);
static int left_comments_parse();
static int not_infixed_parse(int *node);
static int get_left_comment(int node, int *left_comment, int *i);
static int is_multiline_block_comment(int node);
static int is_empty_block_comment(int node);
static int is_line_comment(int node);
static void right_comments_with_title_write(int node, int indent);
static int is_multiline_comment(int node);
static int argument_parse(int *node);
static int triple_string_mask_any_char_parse(uint8_t *c, int *i);
static void not_infixed_write(int node, int indent);

static char *PATH;
// Twice the size of the largest Elm file I have seen.
#define MAX_SRC 1400 * 1000
static uint8_t SRC[MAX_SRC];
static uint8_t TRIPLE_STRING_MASK[MAX_SRC];
static uint16_t ROW[MAX_SRC];
static uint16_t COLUMN[MAX_SRC];
static int NUM_SRC = 0;
static int I = 0;

static uint8_t OUT[MAX_SRC];
static int NUM_OUT = 0;

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

#define MAX_RIGHT_COMMENT 10000
static uint32_t RIGHT_COMMENT[MAX_RIGHT_COMMENT];
static uint32_t RIGHT_COMMENT_PARENT[MAX_RIGHT_COMMENT];
int NUM_RIGHT_COMMENT = 0;

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

#define MAX_EMPTY_TUPLE 10000
static uint32_t IS_EMPTY_TUPLE[MAX_EMPTY_TUPLE];
int NUM_EMPTY_TUPLE = 0;

#define MAX_EMPTY_RECORD 10000
static uint32_t IS_EMPTY_RECORD[MAX_EMPTY_RECORD];
int NUM_EMPTY_RECORD = 0;

#define MAX_LIST_ITEM 10000
static uint32_t LIST_ITEM[MAX_LIST_ITEM];
static uint32_t LIST[MAX_LIST_ITEM];
int NUM_LIST_ITEM = 0;

#define MAX_TUPLE_ITEM 10000
static uint32_t TUPLE_ITEM[MAX_TUPLE_ITEM];
static uint32_t TUPLE[MAX_TUPLE_ITEM];
int NUM_TUPLE_ITEM = 0;

#define MAX_SRC_MULTILINE 10000
static uint32_t SRC_MULTILINE[MAX_SRC_MULTILINE];
int NUM_SRC_MULTILINE = 0;

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

#define MAX_IS_ARG1_LINE1 10000
static uint32_t IS_ARG1_LINE1[MAX_IS_ARG1_LINE1];
int NUM_IS_ARG1_LINE1 = 0;

#define MAX_PLUS 10000
static uint32_t PLUS_LEFT[MAX_PLUS];
static uint32_t PLUS_RIGHT[MAX_PLUS];
int NUM_PLUS = 0;

#define MAX_MINUS 10000
static uint32_t MINUS_LEFT[MAX_MINUS];
static uint32_t MINUS_RIGHT[MAX_MINUS];
int NUM_MINUS = 0;

#define MAX_ASTERISK 10000
static uint32_t ASTERISK_LEFT[MAX_ASTERISK];
static uint32_t ASTERISK_RIGHT[MAX_ASTERISK];
int NUM_ASTERISK = 0;

#define MAX_DIVIDE 10000
static uint32_t DIVIDE_LEFT[MAX_DIVIDE];
static uint32_t DIVIDE_RIGHT[MAX_DIVIDE];
int NUM_DIVIDE = 0;

#define MAX_POWER 10000
static uint32_t POWER_LEFT[MAX_POWER];
static uint32_t POWER_RIGHT[MAX_POWER];
int NUM_POWER = 0;

#define MAX_GREATER_THAN 10000
static uint32_t GREATER_THAN_LEFT[MAX_GREATER_THAN];
static uint32_t GREATER_THAN_RIGHT[MAX_GREATER_THAN];
int NUM_GREATER_THAN = 0;

#define MAX_LESS_THAN 10000
static uint32_t LESS_THAN_LEFT[MAX_LESS_THAN];
static uint32_t LESS_THAN_RIGHT[MAX_LESS_THAN];
int NUM_LESS_THAN = 0;

#define MAX_INT_DIVIDE 10000
static uint32_t INT_DIVIDE_LEFT[MAX_INT_DIVIDE];
static uint32_t INT_DIVIDE_RIGHT[MAX_INT_DIVIDE];
int NUM_INT_DIVIDE = 0;

#define MAX_TRIPLE_STRING_ITEM 10000
static uint32_t TRIPLE_STRING[MAX_TRIPLE_STRING_ITEM];
static uint32_t TRIPLE_STRING_ITEM[MAX_TRIPLE_STRING_ITEM];
int NUM_TRIPLE_STRING_ITEM = 0;

#define MAX_EMPTY_TRIPLE_STRING 10000
static uint32_t EMPTY_TRIPLE_STRING[MAX_EMPTY_TRIPLE_STRING];
int NUM_EMPTY_TRIPLE_STRING = 0;

#define MAX_EMPTY_NORMAL_STRING 10000
static uint32_t EMPTY_NORMAL_STRING[MAX_EMPTY_NORMAL_STRING];
int NUM_EMPTY_NORMAL_STRING = 0;

#define MAX_NORMAL_STRING_ITEM 10000
static uint32_t NORMAL_STRING[MAX_NORMAL_STRING_ITEM];
static uint32_t NORMAL_STRING_ITEM[MAX_NORMAL_STRING_ITEM];
int NUM_NORMAL_STRING_ITEM = 0;

#define MAX_PLUS_PLUS 10000
static uint32_t PLUS_PLUS_LEFT[MAX_PLUS_PLUS];
static uint32_t PLUS_PLUS_RIGHT[MAX_PLUS_PLUS];
int NUM_PLUS_PLUS = 0;

#define MAX_CONS 10000
static uint32_t CONS_LEFT[MAX_CONS];
static uint32_t CONS_RIGHT[MAX_CONS];
int NUM_CONS = 0;

#define MAX_GREATER_THAN_OR_EQUAL 10000
static uint32_t GREATER_THAN_OR_EQUAL_LEFT[MAX_GREATER_THAN_OR_EQUAL];
static uint32_t GREATER_THAN_OR_EQUAL_RIGHT[MAX_GREATER_THAN_OR_EQUAL];
int NUM_GREATER_THAN_OR_EQUAL = 0;

#define MAX_LESS_THAN_OR_EQUAL 10000
static uint32_t LESS_THAN_OR_EQUAL_LEFT[MAX_LESS_THAN_OR_EQUAL];
static uint32_t LESS_THAN_OR_EQUAL_RIGHT[MAX_LESS_THAN_OR_EQUAL];
int NUM_LESS_THAN_OR_EQUAL = 0;

#define MAX_EQUAL 10000
static uint32_t EQUAL_LEFT[MAX_EQUAL];
static uint32_t EQUAL_RIGHT[MAX_EQUAL];
int NUM_EQUAL = 0;

#define MAX_OR 10000
static uint32_t OR_LEFT[MAX_OR];
static uint32_t OR_RIGHT[MAX_OR];
int NUM_OR = 0;

#define MAX_AND 10000
static uint32_t AND_LEFT[MAX_AND];
static uint32_t AND_RIGHT[MAX_AND];
int NUM_AND = 0;

#define MAX_NOT_EQUAL 10000
static uint32_t NOT_EQUAL_LEFT[MAX_NOT_EQUAL];
static uint32_t NOT_EQUAL_RIGHT[MAX_NOT_EQUAL];
int NUM_NOT_EQUAL = 0;

#define MAX_PARSE_DOT 10000
static uint32_t PARSE_DOT_LEFT[MAX_PARSE_DOT];
static uint32_t PARSE_DOT_RIGHT[MAX_PARSE_DOT];
int NUM_PARSE_DOT = 0;

#define MAX_PARSE_EQUALS 10000
static uint32_t PARSE_EQUALS_LEFT[MAX_PARSE_EQUALS];
static uint32_t PARSE_EQUALS_RIGHT[MAX_PARSE_EQUALS];
int NUM_PARSE_EQUALS = 0;

#define MAX_COMPOSE_LEFT 10000
static uint32_t COMPOSE_LEFT_LEFT[MAX_COMPOSE_LEFT];
static uint32_t COMPOSE_LEFT_RIGHT[MAX_COMPOSE_LEFT];
int NUM_COMPOSE_LEFT = 0;

#define MAX_COMPOSE_RIGHT 10000
static uint32_t COMPOSE_RIGHT_LEFT[MAX_COMPOSE_RIGHT];
static uint32_t COMPOSE_RIGHT_RIGHT[MAX_COMPOSE_RIGHT];
int NUM_COMPOSE_RIGHT = 0;

#define MAX_LEFT_PIZZA 10000
static uint32_t LEFT_PIZZA_LEFT[MAX_LEFT_PIZZA];
static uint32_t LEFT_PIZZA_RIGHT[MAX_LEFT_PIZZA];
int NUM_LEFT_PIZZA = 0;

#define MAX_MULTILINE_INFIX 10000
static uint32_t MULTILINE_INFIX[MAX_MULTILINE_INFIX];
int NUM_MULTILINE_INFIX = 0;

#define MAX_RIGHT_PIZZA 10000
static uint32_t RIGHT_PIZZA_LEFT[MAX_RIGHT_PIZZA];
static uint32_t RIGHT_PIZZA_RIGHT[MAX_RIGHT_PIZZA];
int NUM_RIGHT_PIZZA = 0;

#define MAX_IN_PARENS 10000
static uint32_t IN_PARENS[MAX_IN_PARENS];
static uint32_t IN_PARENS_PARENT[MAX_IN_PARENS];
int NUM_IN_PARENS = 0;

#define MAX_RECORD_FIELD 10000
static uint32_t RECORD_FIELD[MAX_RECORD_FIELD];
static uint32_t RECORD_FIELD_NAME[MAX_RECORD_FIELD];
static uint32_t RECORD_FIELD_VALUE[MAX_RECORD_FIELD];
int NUM_RECORD_FIELD = 0;

#define MAX_RECORD_ITEM 10000
static uint32_t RECORD[MAX_RECORD_ITEM];
static uint32_t RECORD_ITEM[MAX_RECORD_ITEM];
int NUM_RECORD_ITEM = 0;

#define MAX_RECORD_UPDATE_NAME 10000
static uint32_t RECORD_UPDATE[MAX_RECORD_UPDATE_NAME];
static uint32_t RECORD_UPDATE_NAME[MAX_RECORD_UPDATE_NAME];
int NUM_RECORD_UPDATE_NAME = 0;

#define MAX_DOT_FUNCTION 10000
static uint32_t DOT_FUNCTION[MAX_DOT_FUNCTION];
static uint32_t DOT_FUNCTION_NAME[MAX_DOT_FUNCTION];
int NUM_DOT_FUNCTION = 0;

#define MAX_DOTTED 10000
static uint32_t DOTTED_HEAD[MAX_DOTTED];
static uint32_t DOTTED_TAIL[MAX_DOTTED];
int NUM_DOTTED = 0;

#define MAX_IF_THEN_ELSE 10000
static uint32_t IF_THEN_ELSE[MAX_IF_THEN_ELSE];
static uint32_t IF_THEN_ELSE_CONDITION[MAX_IF_THEN_ELSE];
static uint32_t IF_THEN_ELSE_THEN[MAX_IF_THEN_ELSE];
static uint32_t IF_THEN_ELSE_ELSE[MAX_IF_THEN_ELSE];
int NUM_IF_THEN_ELSE = 0;

#define MAX_CASE_BRANCH 10000
static uint32_t CASE_OF_BRANCH_PARENT[MAX_CASE_BRANCH];
static uint32_t CASE_OF_BRANCH_LEFT[MAX_CASE_BRANCH];
static uint32_t CASE_OF_BRANCH_RIGHT[MAX_CASE_BRANCH];
int NUM_CASE_BRANCH = 0;

#define MAX_CASE_OF 10000
static uint32_t CASE_OF[MAX_CASE_OF];
static uint32_t CASE_OF_PIVOT[MAX_CASE_OF];
int NUM_CASE_OF = 0;

static void attach_case_of_branch(int node, int branch) {
  for (int i = 0; i < NUM_CASE_BRANCH; ++i) {
    if (CASE_OF_BRANCH_PARENT[i] == (uint32_t)branch) {
      CASE_OF_BRANCH_PARENT[i] = node;
    }
  }
}

static void append_case_of(int node, int pivot) {
  if (NUM_CASE_OF == MAX_CASE_OF) {
    fprintf(stderr, "%s: too many case of nodes, maximum is %d\n", PATH,
            MAX_CASE_OF);
    exit(-1);
  }
  CASE_OF[NUM_CASE_OF] = node;
  CASE_OF_PIVOT[NUM_CASE_OF] = pivot;
  ++NUM_CASE_OF;
}

static void append_case_branch(int parent, int left, int right) {
  if (NUM_CASE_BRANCH == MAX_CASE_BRANCH) {
    fprintf(stderr, "%s: too many case branches, maximum is %d\n", PATH,
            MAX_CASE_BRANCH);
    exit(-1);
  }
  CASE_OF_BRANCH_PARENT[NUM_CASE_BRANCH] = parent;
  CASE_OF_BRANCH_LEFT[NUM_CASE_BRANCH] = left;
  CASE_OF_BRANCH_RIGHT[NUM_CASE_BRANCH] = right;
  ++NUM_CASE_BRANCH;
}

static void append_if_then_else(int node, int condition, int then_branch,
                                int else_branch) {
  if (NUM_IF_THEN_ELSE == MAX_IF_THEN_ELSE) {
    fprintf(stderr, "%s: too many if then else nodes, maximum is %d\n", PATH,
            MAX_IF_THEN_ELSE);
    exit(-1);
  }
  IF_THEN_ELSE[NUM_IF_THEN_ELSE] = node;
  IF_THEN_ELSE_CONDITION[NUM_IF_THEN_ELSE] = condition;
  IF_THEN_ELSE_THEN[NUM_IF_THEN_ELSE] = then_branch;
  IF_THEN_ELSE_ELSE[NUM_IF_THEN_ELSE] = else_branch;
  ++NUM_IF_THEN_ELSE;
}

static void append_dotted(int head, int tail) {
  if (NUM_DOTTED == MAX_DOTTED) {
    fprintf(stderr, "%s: too many dotted nodes, maximum is %d\n", PATH,
            MAX_DOTTED);
    exit(-1);
  }
  DOTTED_HEAD[NUM_DOTTED] = head;
  DOTTED_TAIL[NUM_DOTTED] = tail;
  ++NUM_DOTTED;
}

static void append_dot_function(int node, int name) {
  if (NUM_DOT_FUNCTION == MAX_DOT_FUNCTION) {
    fprintf(stderr, "%s: too many dot functions, maximum is %d\n", PATH,
            MAX_DOT_FUNCTION);
    exit(-1);
  }
  DOT_FUNCTION_NAME[NUM_DOT_FUNCTION] = name;
  DOT_FUNCTION[NUM_DOT_FUNCTION] = node;
  ++NUM_DOT_FUNCTION;
}

static void append_record_update_name(int node, int name) {
  if (NUM_RECORD_UPDATE_NAME == MAX_RECORD_UPDATE_NAME) {
    fprintf(stderr, "%s: too many record update names, maximum is %d\n", PATH,
            MAX_RECORD_UPDATE_NAME);
    exit(-1);
  }
  RECORD_UPDATE_NAME[NUM_RECORD_UPDATE_NAME] = name;
  RECORD_UPDATE[NUM_RECORD_UPDATE_NAME] = node;
  ++NUM_RECORD_UPDATE_NAME;
}

static void append_record_item(int node, int item) {
  if (NUM_RECORD_ITEM == MAX_RECORD_ITEM) {
    fprintf(stderr, "%s: too many record items, maximum is %d\n", PATH,
            MAX_RECORD_ITEM);
    exit(-1);
  }
  RECORD_ITEM[NUM_RECORD_ITEM] = item;
  RECORD[NUM_RECORD_ITEM] = node;
  ++NUM_RECORD_ITEM;
}

static void append_record_field(int node, int name, int value) {
  if (NUM_RECORD_FIELD == MAX_RECORD_FIELD) {
    fprintf(stderr, "%s: too many record fields, maximum is %d\n", PATH,
            MAX_RECORD_FIELD);
    exit(-1);
  }
  RECORD_FIELD[NUM_RECORD_FIELD] = node;
  RECORD_FIELD_NAME[NUM_RECORD_FIELD] = name;
  RECORD_FIELD_VALUE[NUM_RECORD_FIELD] = value;
  ++NUM_RECORD_FIELD;
}

static int num_left_comments(int node) {
  int num = 0;
  for (int i = 0; i < NUM_LEFT_COMMENT; ++i) {
    if (LEFT_COMMENT_PARENT[i] == (uint32_t)node) {
      ++num;
    }
  }
  return num;
}

static int num_right_comments(int node) {
  int num = 0;
  for (int i = 0; i < NUM_RIGHT_COMMENT; ++i) {
    if (RIGHT_COMMENT_PARENT[i] == (uint32_t)node) {
      ++num;
    }
  }
  return num;
}

static void append_in_parens(int node, int expression) {
  if (NUM_IN_PARENS == MAX_IN_PARENS) {
    fprintf(stderr, "%s: too many in parens nodes, maximum is %d\n", PATH,
            MAX_IN_PARENS);
    exit(-1);
  }
  IN_PARENS[NUM_IN_PARENS] = expression;
  IN_PARENS_PARENT[NUM_IN_PARENS] = node;
  ++NUM_IN_PARENS;
}

static void append_right_pizza(int left, int right) {
  if (NUM_RIGHT_PIZZA == MAX_RIGHT_PIZZA) {
    fprintf(stderr, "%s: too many right pizza nodes, maximum is %d\n", PATH,
            MAX_RIGHT_PIZZA);
    exit(-1);
  }
  RIGHT_PIZZA_LEFT[NUM_RIGHT_PIZZA] = left;
  RIGHT_PIZZA_RIGHT[NUM_RIGHT_PIZZA] = right;
  ++NUM_RIGHT_PIZZA;
}

static void append_multiline_infix(int node) {
  if (NUM_MULTILINE_INFIX == MAX_MULTILINE_INFIX) {
    fprintf(stderr, "%s: too many multiline infix nodes, maximum is %d\n", PATH,
            MAX_MULTILINE_INFIX);
    exit(-1);
  }
  MULTILINE_INFIX[NUM_MULTILINE_INFIX] = node;
  ++NUM_MULTILINE_INFIX;
}

static void append_left_pizza(int left, int right) {
  if (NUM_LEFT_PIZZA == MAX_LEFT_PIZZA) {
    fprintf(stderr, "%s: too many left pizza nodes, maximum is %d\n", PATH,
            MAX_LEFT_PIZZA);
    exit(-1);
  }
  LEFT_PIZZA_LEFT[NUM_LEFT_PIZZA] = left;
  LEFT_PIZZA_RIGHT[NUM_LEFT_PIZZA] = right;
  ++NUM_LEFT_PIZZA;
}

static void append_compose_right(int left, int right) {
  if (NUM_COMPOSE_RIGHT == MAX_COMPOSE_RIGHT) {
    fprintf(stderr, "%s: too many compose right nodes, maximum is %d\n", PATH,
            MAX_COMPOSE_RIGHT);
    exit(-1);
  }
  COMPOSE_RIGHT_LEFT[NUM_COMPOSE_RIGHT] = left;
  COMPOSE_RIGHT_RIGHT[NUM_COMPOSE_RIGHT] = right;
  ++NUM_COMPOSE_RIGHT;
}

static void append_compose_left(int left, int right) {
  if (NUM_COMPOSE_LEFT == MAX_COMPOSE_LEFT) {
    fprintf(stderr, "%s: too many compose left nodes, maximum is %d\n", PATH,
            MAX_COMPOSE_LEFT);
    exit(-1);
  }
  COMPOSE_LEFT_LEFT[NUM_COMPOSE_LEFT] = left;
  COMPOSE_LEFT_RIGHT[NUM_COMPOSE_LEFT] = right;
  ++NUM_COMPOSE_LEFT;
}

static void append_parse_equals(int left, int right) {
  if (NUM_PARSE_EQUALS == MAX_PARSE_EQUALS) {
    fprintf(stderr, "%s: too many parse equal nodes, maximum is %d\n", PATH,
            MAX_PARSE_EQUALS);
    exit(-1);
  }
  PARSE_EQUALS_LEFT[NUM_PARSE_EQUALS] = left;
  PARSE_EQUALS_RIGHT[NUM_PARSE_EQUALS] = right;
  ++NUM_PARSE_EQUALS;
}

static void append_parse_dot(int left, int right) {
  if (NUM_PARSE_DOT == MAX_PARSE_DOT) {
    fprintf(stderr, "%s: too many parse dot nodes, maximum is %d\n", PATH,
            MAX_PARSE_DOT);
    exit(-1);
  }
  PARSE_DOT_LEFT[NUM_PARSE_DOT] = left;
  PARSE_DOT_RIGHT[NUM_PARSE_DOT] = right;
  ++NUM_PARSE_DOT;
}

static void append_not_equal(int left, int right) {
  if (NUM_NOT_EQUAL == MAX_NOT_EQUAL) {
    fprintf(stderr, "%s: too many not equal nodes, maximum is %d\n", PATH,
            MAX_NOT_EQUAL);
    exit(-1);
  }
  NOT_EQUAL_LEFT[NUM_NOT_EQUAL] = left;
  NOT_EQUAL_RIGHT[NUM_NOT_EQUAL] = right;
  ++NUM_NOT_EQUAL;
}

static void append_and(int left, int right) {
  if (NUM_AND == MAX_AND) {
    fprintf(stderr, "%s: too many and nodes, maximum is %d\n", PATH, MAX_AND);
    exit(-1);
  }
  AND_LEFT[NUM_AND] = left;
  AND_RIGHT[NUM_AND] = right;
  ++NUM_AND;
}

static void append_or(int left, int right) {
  if (NUM_OR == MAX_OR) {
    fprintf(stderr, "%s: too many or nodes, maximum is %d\n", PATH, MAX_OR);
    exit(-1);
  }
  OR_LEFT[NUM_OR] = left;
  OR_RIGHT[NUM_OR] = right;
  ++NUM_OR;
}

static void append_less_than_or_equal(int left, int right) {
  if (NUM_LESS_THAN_OR_EQUAL == MAX_LESS_THAN_OR_EQUAL) {
    fprintf(stderr, "%s: too many less than or equal nodes, maximum is %d\n",
            PATH, MAX_LESS_THAN_OR_EQUAL);
    exit(-1);
  }
  LESS_THAN_OR_EQUAL_LEFT[NUM_LESS_THAN_OR_EQUAL] = left;
  LESS_THAN_OR_EQUAL_RIGHT[NUM_LESS_THAN_OR_EQUAL] = right;
  ++NUM_LESS_THAN_OR_EQUAL;
}

static void append_greater_than_or_equal(int left, int right) {
  if (NUM_GREATER_THAN_OR_EQUAL == MAX_GREATER_THAN_OR_EQUAL) {
    fprintf(stderr, "%s: too many greater than or equal nodes, maximum is %d\n",
            PATH, MAX_GREATER_THAN_OR_EQUAL);
    exit(-1);
  }
  GREATER_THAN_OR_EQUAL_LEFT[NUM_GREATER_THAN_OR_EQUAL] = left;
  GREATER_THAN_OR_EQUAL_RIGHT[NUM_GREATER_THAN_OR_EQUAL] = right;
  ++NUM_GREATER_THAN_OR_EQUAL;
}

static void append_plus_plus(int left, int right) {
  if (NUM_PLUS_PLUS == MAX_PLUS_PLUS) {
    fprintf(stderr, "%s: too many plus plus nodes, maximum is %d\n", PATH,
            MAX_PLUS_PLUS);
    exit(-1);
  }
  PLUS_PLUS_LEFT[NUM_PLUS_PLUS] = left;
  PLUS_PLUS_RIGHT[NUM_PLUS_PLUS] = right;
  ++NUM_PLUS_PLUS;
}

static void append_cons(int left, int right) {
  if (NUM_CONS == MAX_CONS) {
    fprintf(stderr, "%s: too many cons nodes, maximum is %d\n", PATH, MAX_CONS);
    exit(-1);
  }
  CONS_LEFT[NUM_CONS] = left;
  CONS_RIGHT[NUM_CONS] = right;
  ++NUM_CONS;
}

static void append_normal_string_item(int node, int item) {
  if (NUM_NORMAL_STRING_ITEM == MAX_NORMAL_STRING_ITEM) {
    fprintf(stderr, "%s: too many normal string items, maximum is %d\n", PATH,
            MAX_NORMAL_STRING_ITEM);
    exit(-1);
  }
  NORMAL_STRING[NUM_NORMAL_STRING_ITEM] = node;
  NORMAL_STRING_ITEM[NUM_NORMAL_STRING_ITEM] = item;
  ++NUM_NORMAL_STRING_ITEM;
}

static void append_empty_normal_string(int node) {
  if (NUM_EMPTY_NORMAL_STRING == MAX_EMPTY_NORMAL_STRING) {
    fprintf(stderr, "%s: too many empty normal string nodes, maximum is %d\n",
            PATH, MAX_EMPTY_NORMAL_STRING);
    exit(-1);
  }
  EMPTY_NORMAL_STRING[NUM_EMPTY_NORMAL_STRING] = node;
  ++NUM_EMPTY_NORMAL_STRING;
}

static void append_empty_triple_string(int node) {
  if (NUM_EMPTY_TRIPLE_STRING == MAX_EMPTY_TRIPLE_STRING) {
    fprintf(stderr, "%s: too many empty triple string nodes, maximum is %d\n",
            PATH, MAX_EMPTY_TRIPLE_STRING);
    exit(-1);
  }
  EMPTY_TRIPLE_STRING[NUM_EMPTY_TRIPLE_STRING] = node;
  ++NUM_EMPTY_TRIPLE_STRING;
}

static int is_empty_triple_string(int node) {
  for (int i = 0; i < NUM_EMPTY_TRIPLE_STRING; ++i) {
    if (EMPTY_TRIPLE_STRING[i] == (uint32_t)node) {
      return 1;
    }
  }
  return 0;
}

static void append_triple_string_item(int node, int item) {
  if (NUM_TRIPLE_STRING_ITEM == MAX_TRIPLE_STRING_ITEM) {
    fprintf(stderr, "%s: too many triple string items, maximum is %d\n", PATH,
            MAX_TRIPLE_STRING_ITEM);
    exit(-1);
  }
  TRIPLE_STRING[NUM_TRIPLE_STRING_ITEM] = node;
  TRIPLE_STRING_ITEM[NUM_TRIPLE_STRING_ITEM] = item;
  ++NUM_TRIPLE_STRING_ITEM;
}

static void char_write(uint8_t c) {
  if (NUM_OUT == MAX_SRC) {
    fprintf(stderr, "%s: too many output characters, maximum is %d\n", PATH,
            MAX_SRC);
    exit(-1);
  }
  OUT[NUM_OUT] = c;
  ++NUM_OUT;
}

static void chunk_write(char *chunk) {
  for (int i = 0; chunk[i] != '\0'; ++i) {
    char_write(chunk[i]);
  }
}

static void append_int_divide(int left, int right) {
  if (NUM_INT_DIVIDE == MAX_INT_DIVIDE) {
    fprintf(stderr, "%s: too many int divide nodes, maximum is %d\n", PATH,
            MAX_INT_DIVIDE);
    exit(-1);
  }
  INT_DIVIDE_LEFT[NUM_INT_DIVIDE] = left;
  INT_DIVIDE_RIGHT[NUM_INT_DIVIDE] = right;
  ++NUM_INT_DIVIDE;
}

static void append_less_than(int left, int right) {
  if (NUM_LESS_THAN == MAX_LESS_THAN) {
    fprintf(stderr, "%s: too many less than nodes, maximum is %d\n", PATH,
            MAX_LESS_THAN);
    exit(-1);
  }
  LESS_THAN_LEFT[NUM_LESS_THAN] = left;
  LESS_THAN_RIGHT[NUM_LESS_THAN] = right;
  ++NUM_LESS_THAN;
}

static void append_equal(int left, int right) {
  if (NUM_EQUAL == MAX_EQUAL) {
    fprintf(stderr, "%s: too many equal nodes, maximum is %d\n", PATH,
            MAX_EQUAL);
    exit(-1);
  }
  EQUAL_LEFT[NUM_EQUAL] = left;
  EQUAL_RIGHT[NUM_EQUAL] = right;
  ++NUM_EQUAL;
}

static void append_greater_than(int left, int right) {
  if (NUM_GREATER_THAN == MAX_GREATER_THAN) {
    fprintf(stderr, "%s: too many greater than nodes, maximum is %d\n", PATH,
            MAX_GREATER_THAN);
    exit(-1);
  }
  GREATER_THAN_LEFT[NUM_GREATER_THAN] = left;
  GREATER_THAN_RIGHT[NUM_GREATER_THAN] = right;
  ++NUM_GREATER_THAN;
}

static void append_power(int left, int right) {
  if (NUM_POWER == MAX_POWER) {
    fprintf(stderr, "%s: too many power nodes, maximum is %d\n", PATH,
            MAX_POWER);
    exit(-1);
  }
  POWER_LEFT[NUM_POWER] = left;
  POWER_RIGHT[NUM_POWER] = right;
  ++NUM_POWER;
}

static void append_divide(int left, int right) {
  if (NUM_DIVIDE == MAX_DIVIDE) {
    fprintf(stderr, "%s: too many divide nodes, maximum is %d\n", PATH,
            MAX_DIVIDE);
    exit(-1);
  }
  DIVIDE_LEFT[NUM_DIVIDE] = left;
  DIVIDE_RIGHT[NUM_DIVIDE] = right;
  ++NUM_DIVIDE;
}

static void append_asterisk(int left, int right) {
  if (NUM_ASTERISK == MAX_ASTERISK) {
    fprintf(stderr, "%s: too many asterisk nodes, maximum is %d\n", PATH,
            MAX_ASTERISK);
    exit(-1);
  }
  ASTERISK_LEFT[NUM_ASTERISK] = left;
  ASTERISK_RIGHT[NUM_ASTERISK] = right;
  ++NUM_ASTERISK;
}

static void append_minus(int left, int right) {
  if (NUM_MINUS == MAX_MINUS) {
    fprintf(stderr, "%s: too many minus nodes, maximum is %d\n", PATH,
            MAX_MINUS);
    exit(-1);
  }
  MINUS_LEFT[NUM_MINUS] = left;
  MINUS_RIGHT[NUM_MINUS] = right;
  ++NUM_MINUS;
}

static void append_plus(int left, int right) {
  if (NUM_PLUS == MAX_PLUS) {
    fprintf(stderr, "%s: too many plus nodes, maximum is %d\n", PATH, MAX_PLUS);
    exit(-1);
  }
  PLUS_LEFT[NUM_PLUS] = left;
  PLUS_RIGHT[NUM_PLUS] = right;
  ++NUM_PLUS;
}

static void append_right_comment(int node, int comment_node) {
  if (NUM_RIGHT_COMMENT == MAX_RIGHT_COMMENT) {
    fprintf(stderr, "%s: too many right comment nodes, maximum is %d\n", PATH,
            MAX_RIGHT_COMMENT);
    exit(-1);
  }
  RIGHT_COMMENT[NUM_RIGHT_COMMENT] = comment_node;
  RIGHT_COMMENT_PARENT[NUM_RIGHT_COMMENT] = node;
  ++NUM_RIGHT_COMMENT;
}

static void attach_right_comment_in_expression(int node, int right_comment) {
  for (int i = 0; i < NUM_SAME_LINE_COMMENT; ++i) {
    if (SAME_LINE_COMMENT_PARENT[i] == (uint32_t)right_comment) {
      SAME_LINE_COMMENT_PARENT[i] = node;
    }
  }
  for (int i = 0; i < NUM_RIGHT_COMMENT; ++i) {
    if (RIGHT_COMMENT_PARENT[i] == (uint32_t)right_comment) {
      RIGHT_COMMENT_PARENT[i] = node;
    }
  }
}

static void append_is_arg1_line1(int node) {
  if (NUM_IS_ARG1_LINE1 == MAX_IS_ARG1_LINE1) {
    fprintf(stderr, "%s: too many single line nodes, maximum is %d\n", PATH,
            MAX_IS_ARG1_LINE1);
    exit(-1);
  }
  IS_ARG1_LINE1[NUM_IS_ARG1_LINE1] = node;
  ++NUM_IS_ARG1_LINE1;
}

static void append_argument(int parent, int child) {
  if (NUM_ARGUMENT == MAX_ARGUMENT) {
    fprintf(stderr, "%s: too many arguments, maximum is %d\n", PATH,
            MAX_ARGUMENT);
    exit(-1);
  }
  ARGUMENT[NUM_ARGUMENT] = child;
  ARGUMENT_PARENT[NUM_ARGUMENT] = parent;
  ++NUM_ARGUMENT;
}

static void append_is_double_hyphen_block_comment(int node) {
  if (NUM_DOUBLE_HYPHEN_BLOCK == MAX_DOUBLE_HYPHEN_BLOCK) {
    fprintf(stderr,
            "%s: too many double hyphen block comment nodes, maximum is %d\n",
            PATH, MAX_DOUBLE_HYPHEN_BLOCK);
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

static int is_multiline_src_node(int node) {
  for (int i = 0; i < NUM_SRC_MULTILINE; ++i) {
    if (SRC_MULTILINE[i] == (uint32_t)node) {
      return 1;
    }
  }
  return 0;
}

static int is_multiline_node(int node) {
  if (is_multiline_src_node(node)) {
    return 1;
  }
  if (is_if_then_else_node(node)) {
    return 1;
  }
  int item;
  int start = 0;
  while (get_list_item(node, &item, &start) == 0) {
    if (is_multiline_node(item)) {
      return 1;
    }
  }
  start = 0;
  while (get_tuple_item(node, &item, &start) == 0) {
    if (is_multiline_node(item)) {
      return 1;
    }
  }
  start = 0;
  while (get_argument(node, &item, &start) == 0) {
    if (is_multiline_node(item)) {
      return 1;
    }
  }
  if (get_in_parens(node, &item)) {
    if (is_multiline_node(item)) {
      return 1;
    }
  }
  return 0;
}

static void append_same_line_comment(int node, int comment) {
  if (NUM_SAME_LINE_COMMENT == MAX_SAME_LINE_COMMENT) {
    fprintf(stderr, "%s: too many same line comment nodes, maximum is %d\n",
            PATH, MAX_SAME_LINE_COMMENT);
    exit(-1);
  }
  SAME_LINE_COMMENT[NUM_SAME_LINE_COMMENT] = comment;
  SAME_LINE_COMMENT_PARENT[NUM_SAME_LINE_COMMENT] = node;
  ++NUM_SAME_LINE_COMMENT;
}

static void append_title_comment(int node, int comment) {
  if (NUM_TITLE_COMMENT == MAX_TITLE_COMMENT) {
    fprintf(stderr, "%s: too many title comment nodes, maximum is %d\n", PATH,
            MAX_TITLE_COMMENT);
    exit(-1);
  }
  TITLE_COMMENT[NUM_TITLE_COMMENT] = comment;
  TITLE_COMMENT_PARENT[NUM_TITLE_COMMENT] = node;
  ++NUM_TITLE_COMMENT;
}

static void append_src_multiline(int node) {
  if (NUM_SRC_MULTILINE == MAX_SRC_MULTILINE) {
    fprintf(stderr, "%s: too many nodes, maximum is %d\n", PATH,
            MAX_SRC_MULTILINE);
    exit(-1);
  }
  SRC_MULTILINE[NUM_SRC_MULTILINE] = node;
  ++NUM_SRC_MULTILINE;
}

static void append_list_item(int parent, int child) {
  if (NUM_LIST_ITEM == MAX_LIST_ITEM) {
    fprintf(stderr, "%s: too many list items, maximum is %d\n", PATH,
            MAX_LIST_ITEM);
    exit(-1);
  }
  LIST_ITEM[NUM_LIST_ITEM] = child;
  LIST[NUM_LIST_ITEM] = parent;
  ++NUM_LIST_ITEM;
}

static void append_tuple_item(int parent, int child) {
  if (NUM_TUPLE_ITEM == MAX_TUPLE_ITEM) {
    fprintf(stderr, "%s: too many tuple items, maximum is %d\n", PATH,
            MAX_TUPLE_ITEM);
    exit(-1);
  }
  TUPLE_ITEM[NUM_TUPLE_ITEM] = child;
  TUPLE[NUM_TUPLE_ITEM] = parent;
  ++NUM_TUPLE_ITEM;
}

static void append_is_empty_tuple(int node) {
  if (NUM_EMPTY_TUPLE == MAX_EMPTY_TUPLE) {
    fprintf(stderr, "%s: too many empty tuple nodes, maximum is %d\n", PATH,
            MAX_EMPTY_TUPLE);
    exit(-1);
  }
  IS_EMPTY_TUPLE[NUM_EMPTY_TUPLE] = node;
  ++NUM_EMPTY_TUPLE;
}

static void append_is_empty_record(int node) {
  if (NUM_EMPTY_RECORD == MAX_EMPTY_RECORD) {
    fprintf(stderr, "%s: too many empty record nodes, maximum is %d\n", PATH,
            MAX_EMPTY_RECORD);
    exit(-1);
  }
  IS_EMPTY_RECORD[NUM_EMPTY_RECORD] = node;
  ++NUM_EMPTY_RECORD;
}

static void append_is_empty_list(int node) {
  if (NUM_EMPTY_LIST == MAX_EMPTY_LIST) {
    fprintf(stderr, "%s: too many empty list nodes, maximum is %d\n", PATH,
            MAX_EMPTY_LIST);
    exit(-1);
  }
  IS_EMPTY_LIST[NUM_EMPTY_LIST] = node;
  ++NUM_EMPTY_LIST;
}

static void append_double_hyphen_start_block(int node) {
  if (NUM_HAS_DOUBLE_HYPHEN_BLOCK == MAX_HAS_DOUBLE_HYPHEN_BLOCK) {
    fprintf(stderr,
            "%s: too many double hyphen start block nodes, maximum is %d\n",
            PATH, MAX_HAS_DOUBLE_HYPHEN_BLOCK);
    exit(-1);
  }
  HAS_DOUBLE_HYPHEN_BLOCK[NUM_HAS_DOUBLE_HYPHEN_BLOCK] = node;
  ++NUM_HAS_DOUBLE_HYPHEN_BLOCK;
}

static void append_block_comment_line(int parent, int start, int size) {
  if (NUM_BLOCK_COMMENT_LINE == MAX_BLOCK_COMMENT_LINE) {
    fprintf(stderr, "%s: too many block comment lines, maximum is %d\n", PATH,
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
    fprintf(stderr, "%s: too many empty block comment nodes, maximum is %d\n",
            PATH, MAX_EMPTY_BLOCK_COMMENT);
    exit(-1);
  }
  IS_EMPTY_BLOCK_COMMENT[NUM_EMPTY_BLOCK_COMMENT] = node;
  ++NUM_EMPTY_BLOCK_COMMENT;
}

static void append_left_comment(int node, int comment_node) {
  if (NUM_LEFT_COMMENT == MAX_LEFT_COMMENT) {
    fprintf(stderr, "%s: too many nodes, maximum is %d\n", PATH,
            MAX_LEFT_COMMENT);
    exit(-1);
  }
  LEFT_COMMENT[NUM_LEFT_COMMENT] = comment_node;
  LEFT_COMMENT_PARENT[NUM_LEFT_COMMENT] = node;
  ++NUM_LEFT_COMMENT;
}

static void append_is_negative(int node) {
  if (NUM_HAS_NEGATIVE == MAX_HAS_NEGATIVE) {
    fprintf(stderr, "%s: too many nodes, maximum is %d\n", PATH,
            MAX_HAS_NEGATIVE);
    exit(-1);
  }
  HAS_NEGATIVE[NUM_HAS_NEGATIVE] = node;
  ++NUM_HAS_NEGATIVE;
}

static void append_has_exponent(int node) {
  if (NUM_HAS_EXPONENT == MAX_HAS_EXPONENT) {
    fprintf(stderr, "%s: too many nodes, maximum is %d\n", PATH,
            MAX_HAS_EXPONENT);
    exit(-1);
  }
  HAS_EXPONENT[NUM_HAS_EXPONENT] = node;
  ++NUM_HAS_EXPONENT;
}

static void append_is_hex(int node) {
  if (NUM_HEX == MAX_HEX) {
    fprintf(stderr, "%s: too many nodes, maximum is %d\n", PATH, MAX_HEX);
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
    fprintf(stderr, "%s: too many nodes, maximum is %d\n", PATH, MAX_HAS_SRC);
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
  fprintf(stderr, "%s: could not find node %d\n", PATH, node);
  exit(-1);
}

static void out_write(uint8_t *src, int size) {
  for (int i = 0; i < size; ++i) {
    char_write(src[i]);
  }
}

static void src_write(int node) {
  const int has_src_index = get_has_src_index(node);
  const int start = SRC_START[has_src_index];
  const int size = SRC_SIZE[has_src_index];
  out_write(SRC + start, size);
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
    char_write('-');
  }
  chunk_write("0x");
  const int src_index = get_has_src_index(node);
  const int start = SRC_START[src_index];
  const int size = SRC_SIZE[src_index];
  if (size % 2 == 1) {
    char_write('0');
  }
  for (int i = start; i < start + size; ++i) {
    char_write(hex_to_uppercase(SRC[i]));
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
  char_write('e');
  if (is_negative(node + 1)) {
    char_write('-');
  }
  src_write(node + 1);
}

static int is_empty_tuple(int node) {
  for (int i = 0; i < NUM_EMPTY_TUPLE; ++i) {
    if (IS_EMPTY_TUPLE[i] == (uint32_t)node) {
      return 1;
    }
  }
  return 0;
}

static int is_empty_record(int node) {
  for (int i = 0; i < NUM_EMPTY_RECORD; ++i) {
    if (IS_EMPTY_RECORD[i] == (uint32_t)node) {
      return 1;
    }
  }
  return 0;
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

static int get_tuple_item(int node, int *item, int *start) {
  for (int i = *start; i < NUM_TUPLE_ITEM; ++i) {
    if (TUPLE[i] == (uint32_t)node) {
      *item = TUPLE_ITEM[i];
      *start = i + 1;
      return 0;
    }
  }
  return -1;
}

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
         is_line_comment(node) || is_double_hyphen_block_comment(node);
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

static int has_multiline_right_comment(int node) {
  int right_comment;
  int start = 0;
  while (get_right_comment(node, &right_comment, &start) == 0) {
    if (is_multiline_comment(right_comment)) {
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
  char_write('\n');
  for (int i = 0; i < indent; ++i) {
    char_write(' ');
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
  left_comments_with_spaces_write(0, item, indent + 2);
  expression_write(item, indent + 2);
  if (has_same_line_comment(item)) {
    char_write(' ');
  }
  right_comments_with_title_write(item, indent);
}

static void tuple_item_write(int item, int indent) {
  left_comments_with_spaces_write(0, item, indent + 2);
  expression_write(item, indent + 2);
  right_comments_with_spaces_write(item, indent + 2);
}

static void non_empty_tuple_write(int node, int indent) {
  chunk_write("( ");
  int item;
  int start = 0;
  if (get_tuple_item(node, &item, &start) == 0) {
    tuple_item_write(item, indent);
  }
  int left_is_multiline = has_multiline_left_comment(item);
  int any_is_multiline = left_is_multiline;
  while (get_tuple_item(node, &item, &start) == 0) {
    left_is_multiline = has_multiline_left_comment(item);
    any_is_multiline = any_is_multiline || has_multiline_left_comment(item) ||
                       is_multiline_node(item);
    if (any_is_multiline || is_multiline_node(node)) {
      indent_write(indent);
    }
    chunk_write(", ");
    tuple_item_write(item, indent);
  }
  if (any_is_multiline || is_multiline_node(node)) {
    indent_write(indent);
  } else {
    char_write(' ');
  }
  char_write(')');
}

static void non_empty_list_write(int node, int indent) {
  chunk_write("[ ");
  int item;
  int start = 0;
  if (get_list_item(node, &item, &start) == 0) {
    list_item_write(item, indent);
  }
  int left_is_multiline = has_multiline_left_comment(item);
  int any_is_multiline = left_is_multiline || is_multiline_node(item);
  while (get_list_item(node, &item, &start) == 0) {
    any_is_multiline = any_is_multiline || has_multiline_left_comment(item) ||
                       is_multiline_node(item);
    if (any_is_multiline || is_multiline_node(node)) {
      indent_write(indent);
    }
    chunk_write(", ");
    list_item_write(item, indent);
  }
  if (any_is_multiline || is_multiline_node(node)) {
    indent_write(indent);
  } else {
    char_write(' ');
  }
  char_write(']');
}

static void right_comments_after_record_name_write(int node, int indent) {
  const int has_multiline_comment_after = has_multiline_right_comment(node);
  const int has_comment_after = has_right_comment(node);
  right_comments_with_spaces_write(node, indent + 2);
  if (has_comment_after && has_multiline_comment_after) {
    indent_write(floor_to_four(indent + 6));
  } else {
    char_write(' ');
  }
}

static void record_item_left_write(int node, int name, int indent) {
  left_comments_with_spaces_write(0, node, indent + 2);
  src_write(name);
  right_comments_after_record_name_write(name, indent);
}

static void record_item_right_write(int node, int name, int value, int indent) {
  const int has_multiline_comment_after_name =
      has_multiline_right_comment(name);
  const int left_is_multiline_value = has_multiline_left_comment(value);
  if (is_multiline_node(node) || left_is_multiline_value ||
      is_multiline_node(value) || has_multiline_comment_after_name) {
    indent_write(floor_to_four(indent + 6));
  } else {
    char_write(' ');
  }
  left_comments_with_spaces_write(0, value, floor_to_four(indent + 6));
  expression_write(value, indent + 4);
  if (has_same_line_comment(value)) {
    char_write(' ');
  }
  right_comments_with_title_write(value, indent);
}

static void record_item_write(int node, int name, int value, int indent) {
  record_item_left_write(node, name, indent);
  char_write('=');
  record_item_right_write(node, name, value, indent);
}

static int get_record_item(int node, int *item, int *name, int *value,
                           int *start) {

  for (int i = *start; i < NUM_RECORD_FIELD; ++i) {
    if (RECORD[i] == (uint32_t)node) {
      *item = RECORD_FIELD[i];
      *name = RECORD_FIELD_NAME[i];
      *value = RECORD_FIELD_VALUE[i];
      *start = i + 1;
      return 0;
    }
  }
  return -1;
}

static int multiline_comment_in_record_item(int item, int name, int value) {
  return has_multiline_left_comment(item) || has_title_comment(value) ||
         has_same_line_comment(value) || has_multiline_right_comment(name) ||
         has_multiline_left_comment(value);
}

static void record_items_write(int node, int indent, int *any_is_multiline,
                               int is_multi) {
  int comment_is_multiline;
  int start = 0;
  int item;
  int field_name;
  int value;
  if (get_record_item(node, &item, &field_name, &value, &start) == 0) {
    comment_is_multiline =
        multiline_comment_in_record_item(item, field_name, value);
    *any_is_multiline = comment_is_multiline || is_multiline_node(value);
    record_item_write(item, field_name, value, indent);
  }
  while (get_record_item(node, &item, &field_name, &value, &start) == 0) {
    comment_is_multiline =
        multiline_comment_in_record_item(item, field_name, value);
    *any_is_multiline =
        *any_is_multiline || comment_is_multiline || is_multiline_node(value);
    if (*any_is_multiline || is_multi) {
      indent_write(indent);
    }
    chunk_write(", ");
    record_item_write(item, field_name, value, indent);
  }
}

static void non_empty_record_write(int node, int indent) {
  chunk_write("{ ");
  int any_is_multiline;
  const int is_multi = is_multiline_node(node);
  record_items_write(node, indent, &any_is_multiline, is_multi);
  if (any_is_multiline || is_multi) {
    indent_write(indent);
  } else {
    char_write(' ');
  }
  char_write('}');
}

static int is_non_empty_tuple(int node) {
  for (int i = 0; i < NUM_TUPLE_ITEM; ++i) {
    if (TUPLE[i] == (uint32_t)node) {
      return 1;
    }
  }
  return 0;
}

static int is_non_empty_list(int node) {
  for (int i = 0; i < NUM_LIST_ITEM; ++i) {
    if (LIST[i] == (uint32_t)node) {
      return 1;
    }
  }
  return 0;
}

static int is_non_empty_record(int node) {
  for (int i = 0; i < NUM_RECORD_ITEM; ++i) {
    if (RECORD[i] == (uint32_t)node) {
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

static int floor_to_four(int x) { return x / 4 * 4; }

static int is_arg1_line1(int node) {
  for (int i = 0; i < NUM_IS_ARG1_LINE1; ++i) {
    if (IS_ARG1_LINE1[i] == (uint32_t)node) {
      return 1;
    }
  }
  return 0;
}

static void argument_write(int is_multi, int argument, int indent) {
  const int left_is_multiline = has_multiline_left_comment(argument);
  if ((is_multi && !is_arg1_line1(argument)) || left_is_multiline ||
      is_multiline_node(argument)) {
    indent_write(floor_to_four(indent + 4));
  } else {
    char_write(' ');
  }
  const int comment_indent = floor_to_four(indent + 4);
  left_comments_with_spaces_write(0, argument, comment_indent);
  expression_write(argument, indent + 4);
}

static void callable_write(int node) {
  int dot_function;
  if (get_dot_function(node, &dot_function)) {
    char_write('.');
    src_write(dot_function);
    return;
  }
  src_write(node);
}

static void function_call_write(int node, int indent) {
  callable_write(node);
  int argument;
  int start = 0;
  get_argument(node, &argument, &start);
  const int is_multi = is_multiline_node(node);
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

static int has_right_comment(int node) {
  for (int i = 0; i < NUM_RIGHT_COMMENT; ++i) {
    if (RIGHT_COMMENT_PARENT[i] == (uint32_t)node) {
      return 1;
    }
  }
  return 0;
}

static void left_pizza_write(int is_multi, int left_is_multi, int right,
                             int indent) {
  const int multi_left_comment = has_multiline_left_comment(right);
  if (multi_left_comment || left_is_multi) {
    indent_write(indent);
  } else {
    char_write(' ');
  }
  left_comments_with_spaces_write(0, right, indent);
  chunk_write("<|");
  if (is_multi) {
    indent_write(floor_to_four(indent + 4));
  } else {
    char_write(' ');
  }
  right_comments_in_expression_write(right, floor_to_four(indent + 4));
  const int has_right = has_right_comment(right);
  const int is_single_right = is_single_line_right_comments(right);
  if (has_right && !is_single_right) {
    indent_write(floor_to_four(indent + 4));
  }
  if (has_right && is_single_right) {
    char_write(' ');
  }
  not_infixed_write(right, floor_to_four(indent + 4));
}

static void whitespace_before_infix_write(int is_multi, int right, int indent) {
  if (is_multi || is_multiline_node(right)) {
    indent_write(floor_to_four(indent + 4));
  } else {
    char_write(' ');
  }
  left_comments_with_spaces_write(0, right, floor_to_four(indent + 4));
}

static void whitespace_after_infix_write(int node, int new_indent) {
  char_write(' ');
  right_comments_in_expression_write(node, new_indent);
  const int has_right = has_right_comment(node);
  const int is_single_right = is_single_line_right_comments(node);
  if (has_right && !is_single_right) {
    indent_write(new_indent);
  }
  if (has_right && is_single_right) {
    char_write(' ');
  }
}

static void expression_after_infix_write(int right, int new_indent) {
  int condition;
  int then_branch;
  int else_branch;
  const int is_if =
      get_if_then_else(right, &condition, &then_branch, &else_branch);
  if (is_if) {
    char_write('(');
    if_then_else_write(0, condition, then_branch, else_branch, new_indent + 1);
    indent_write(new_indent);
    char_write(')');
    return;
  }
  not_infixed_write(right, new_indent);
}

static void infix_write_helper(char *infix, int is_multi, int right,
                               int indent) {
  whitespace_before_infix_write(is_multi, right, indent);
  chunk_write(infix);
  const int new_indent = floor_to_four(indent) + 4 + string_length(infix) + 1;
  whitespace_after_infix_write(right, new_indent);
  expression_after_infix_write(right, new_indent);
}

static int get_plus(int node, int *right) {
  for (int i = 0; i < NUM_PLUS; ++i) {
    if (PLUS_LEFT[i] == (uint32_t)node) {
      *right = PLUS_RIGHT[i];
      return 0;
    }
  }
  return -1;
}

static int get_cons(int node, int *right) {
  for (int i = 0; i < NUM_CONS; ++i) {
    if (CONS_LEFT[i] == (uint32_t)node) {
      *right = CONS_RIGHT[i];
      return 0;
    }
  }
  return -1;
}

static int get_minus(int node, int *right) {
  for (int i = 0; i < NUM_MINUS; ++i) {
    if (MINUS_LEFT[i] == (uint32_t)node) {
      *right = MINUS_RIGHT[i];
      return 0;
    }
  }
  return -1;
}

static int get_asterisk(int node, int *right) {
  for (int i = 0; i < NUM_ASTERISK; ++i) {
    if (ASTERISK_LEFT[i] == (uint32_t)node) {
      *right = ASTERISK_RIGHT[i];
      return 0;
    }
  }
  return -1;
}

static int get_divide(int node, int *right) {
  for (int i = 0; i < NUM_DIVIDE; ++i) {
    if (DIVIDE_LEFT[i] == (uint32_t)node) {
      *right = DIVIDE_RIGHT[i];
      return 0;
    }
  }
  return -1;
}

static int get_power(int node, int *right) {
  for (int i = 0; i < NUM_POWER; ++i) {
    if (POWER_LEFT[i] == (uint32_t)node) {
      *right = POWER_RIGHT[i];
      return 0;
    }
  }
  return -1;
}

static int get_greater_than(int node, int *right) {
  for (int i = 0; i < NUM_GREATER_THAN; ++i) {
    if (GREATER_THAN_LEFT[i] == (uint32_t)node) {
      *right = GREATER_THAN_RIGHT[i];
      return 0;
    }
  }
  return -1;
}

static int get_less_than(int node, int *right) {
  for (int i = 0; i < NUM_LESS_THAN; ++i) {
    if (LESS_THAN_LEFT[i] == (uint32_t)node) {
      *right = LESS_THAN_RIGHT[i];
      return 0;
    }
  }
  return -1;
}

static int get_int_divide(int node, int *right) {
  for (int i = 0; i < NUM_INT_DIVIDE; ++i) {
    if (INT_DIVIDE_LEFT[i] == (uint32_t)node) {
      *right = INT_DIVIDE_RIGHT[i];
      return 0;
    }
  }
  return -1;
}

static int get_plus_plus(int node, int *right) {
  for (int i = 0; i < NUM_PLUS_PLUS; ++i) {
    if (PLUS_PLUS_LEFT[i] == (uint32_t)node) {
      *right = PLUS_PLUS_RIGHT[i];
      return 0;
    }
  }
  return -1;
}

static int get_greater_than_or_equal(int node, int *right) {
  for (int i = 0; i < NUM_GREATER_THAN_OR_EQUAL; ++i) {
    if (GREATER_THAN_OR_EQUAL_LEFT[i] == (uint32_t)node) {
      *right = GREATER_THAN_OR_EQUAL_RIGHT[i];
      return 0;
    }
  }
  return -1;
}

static int get_less_than_or_equal(int node, int *right) {
  for (int i = 0; i < NUM_LESS_THAN_OR_EQUAL; ++i) {
    if (LESS_THAN_OR_EQUAL_LEFT[i] == (uint32_t)node) {
      *right = LESS_THAN_OR_EQUAL_RIGHT[i];
      return 0;
    }
  }
  return -1;
}

static int get_equal(int node, int *right) {
  for (int i = 0; i < NUM_EQUAL; ++i) {
    if (EQUAL_LEFT[i] == (uint32_t)node) {
      *right = EQUAL_RIGHT[i];
      return 0;
    }
  }
  return -1;
}

static int get_or(int node, int *right) {
  for (int i = 0; i < NUM_OR; ++i) {
    if (OR_LEFT[i] == (uint32_t)node) {
      *right = OR_RIGHT[i];
      return 0;
    }
  }
  return -1;
}

static int get_and(int node, int *right) {
  for (int i = 0; i < NUM_AND; ++i) {
    if (AND_LEFT[i] == (uint32_t)node) {
      *right = AND_RIGHT[i];
      return 0;
    }
  }
  return -1;
}

static int get_not_equal(int node, int *right) {
  for (int i = 0; i < NUM_NOT_EQUAL; ++i) {
    if (NOT_EQUAL_LEFT[i] == (uint32_t)node) {
      *right = NOT_EQUAL_RIGHT[i];
      return 0;
    }
  }
  return -1;
}

static int get_parse_dot(int node, int *right) {
  for (int i = 0; i < NUM_PARSE_DOT; ++i) {
    if (PARSE_DOT_LEFT[i] == (uint32_t)node) {
      *right = PARSE_DOT_RIGHT[i];
      return 0;
    }
  }
  return -1;
}

static int get_parse_equals(int node, int *right) {
  for (int i = 0; i < NUM_PARSE_EQUALS; ++i) {
    if (PARSE_EQUALS_LEFT[i] == (uint32_t)node) {
      *right = PARSE_EQUALS_RIGHT[i];
      return 0;
    }
  }
  return -1;
}

static int get_compose_left(int node, int *right) {
  for (int i = 0; i < NUM_COMPOSE_LEFT; ++i) {
    if (COMPOSE_LEFT_LEFT[i] == (uint32_t)node) {
      *right = COMPOSE_LEFT_RIGHT[i];
      return 0;
    }
  }
  return -1;
}

static int get_compose_right(int node, int *right) {
  for (int i = 0; i < NUM_COMPOSE_RIGHT; ++i) {
    if (COMPOSE_RIGHT_LEFT[i] == (uint32_t)node) {
      *right = COMPOSE_RIGHT_RIGHT[i];
      return 0;
    }
  }
  return -1;
}

static int get_right_pizza(int node, int *right) {
  for (int i = 0; i < NUM_RIGHT_PIZZA; ++i) {
    if (RIGHT_PIZZA_LEFT[i] == (uint32_t)node) {
      *right = RIGHT_PIZZA_RIGHT[i];
      return 0;
    }
  }
  return -1;
}

static int get_left_pizza(int node, int *right) {
  for (int i = 0; i < NUM_LEFT_PIZZA; ++i) {
    if (LEFT_PIZZA_LEFT[i] == (uint32_t)node) {
      *right = LEFT_PIZZA_RIGHT[i];
      return 0;
    }
  }
  return -1;
}

static int aligned_infix_write(int *left, int is_multi, int indent) {
  int right;
  if (get_cons(*left, &right) == 0) {
    infix_write_helper("::", is_multi, right, indent);
    *left = right;
    return 0;
  }
  if (get_plus(*left, &right) == 0) {
    infix_write_helper("+", is_multi, right, indent);
    *left = right;
    return 0;
  }
  if (get_minus(*left, &right) == 0) {
    infix_write_helper("-", is_multi, right, indent);
    *left = right;
    return 0;
  }
  if (get_asterisk(*left, &right) == 0) {
    infix_write_helper("*", is_multi, right, indent);
    *left = right;
    return 0;
  }
  if (get_divide(*left, &right) == 0) {
    infix_write_helper("/", is_multi, right, indent);
    *left = right;
    return 0;
  }
  if (get_power(*left, &right) == 0) {
    infix_write_helper("^", is_multi, right, indent);
    *left = right;
    return 0;
  }
  if (get_greater_than(*left, &right) == 0) {
    infix_write_helper(">", is_multi, right, indent);
    *left = right;
    return 0;
  }
  if (get_less_than(*left, &right) == 0) {
    infix_write_helper("<", is_multi, right, indent);
    *left = right;
    return 0;
  }
  if (get_int_divide(*left, &right) == 0) {
    infix_write_helper("//", is_multi, right, indent);
    *left = right;
    return 0;
  }
  if (get_plus_plus(*left, &right) == 0) {
    infix_write_helper("++", is_multi, right, indent);
    *left = right;
    return 0;
  }
  if (get_greater_than_or_equal(*left, &right) == 0) {
    infix_write_helper(">=", is_multi, right, indent);
    *left = right;
    return 0;
  }
  if (get_less_than_or_equal(*left, &right) == 0) {
    infix_write_helper("<=", is_multi, right, indent);
    *left = right;
    return 0;
  }
  if (get_equal(*left, &right) == 0) {
    infix_write_helper("==", is_multi, right, indent);
    *left = right;
    return 0;
  }
  if (get_or(*left, &right) == 0) {
    infix_write_helper("||", is_multi, right, indent);
    *left = right;
    return 0;
  }
  if (get_and(*left, &right) == 0) {
    infix_write_helper("&&", is_multi, right, indent);
    *left = right;
    return 0;
  }
  if (get_not_equal(*left, &right) == 0) {
    infix_write_helper("/=", is_multi, right, indent);
    *left = right;
    return 0;
  }
  if (get_parse_dot(*left, &right) == 0) {
    infix_write_helper("|.", is_multi, right, indent);
    *left = right;
    return 0;
  }
  if (get_parse_equals(*left, &right) == 0) {
    infix_write_helper("|=", is_multi, right, indent);
    *left = right;
    return 0;
  }
  if (get_compose_left(*left, &right) == 0) {
    infix_write_helper("<<", is_multi, right, indent);
    *left = right;
    return 0;
  }
  if (get_compose_right(*left, &right) == 0) {
    infix_write_helper(">>", is_multi, right, indent);
    *left = right;
    return 0;
  }
  if (get_right_pizza(*left, &right) == 0) {
    infix_write_helper("|>", is_multi, right, indent);
    *left = right;
    return 0;
  }
  return -1;
}

static void left_pizzas_write(int is_multi, int node, int indent) {
  int left_pizza;
  int pizza_node = node;
  int pizza_indent = indent;
  int left_is_multi = is_multiline_node(pizza_node);
  while (get_left_pizza(pizza_node, &left_pizza) == 0) {
    left_pizza_write(is_multi, left_is_multi, left_pizza, pizza_indent);
    pizza_node = left_pizza;
    left_is_multi = is_multiline_node(pizza_node);
    pizza_indent += 4;
  }
}

static int is_multiline_infix_node(int node) {
  for (int i = 0; i < NUM_MULTILINE_INFIX; ++i) {
    if (MULTILINE_INFIX[i] == (uint32_t)node) {
      return 1;
    }
  }
  return 0;
}

static void expression_write(int node, int indent) {
  not_infixed_write(node, indent);
  const int is_multi = is_multiline_infix_node(node);
  int left = node;
  while (aligned_infix_write(&left, is_multi, indent) == 0) {
  }
  left_pizzas_write(is_multi, node, indent);
}

static int is_non_empty_triple_string(int node) {
  for (int i = 0; i < NUM_TRIPLE_STRING_ITEM; ++i) {
    if (TRIPLE_STRING[i] == (uint32_t)node) {
      return 1;
    }
  }
  return 0;
}

static void string_hex_write(int node) {
  chunk_write("\\u{");
  const int src_index = get_has_src_index(node);
  const int start = SRC_START[src_index];
  const int size = SRC_SIZE[src_index];
  for (int i = start; i < start + size; ++i) {
    char_write(hex_to_uppercase(SRC[i]));
  }
  char_write('}');
}

static void triple_string_item_write(int item) {
  if (is_hex(item)) {
    string_hex_write(item);
    return;
  }
  src_write(item);
}

static int get_triple_string_item(int node, int *item, int *start) {
  for (int i = *start; i < NUM_TRIPLE_STRING_ITEM; ++i) {
    if (TRIPLE_STRING[i] == (uint32_t)node) {
      *item = TRIPLE_STRING_ITEM[i];
      *start = i + 1;
      return 0;
    }
  }
  return -1;
}

static void normal_string_item_write(int item) {
  if (is_hex(item)) {
    string_hex_write(item);
    return;
  }
  src_write(item);
}

static int is_empty_normal_string(int node) {
  for (int i = 0; i < NUM_EMPTY_NORMAL_STRING; ++i) {
    if (EMPTY_NORMAL_STRING[i] == (uint32_t)node) {
      return 1;
    }
  }
  return 0;
}

static void normal_string_write(int node) {
  char_write('"');
  int item;
  int start = 0;
  while (get_normal_string_item(node, &item, &start) == 0) {
    normal_string_item_write(item);
  }
  char_write('"');
}

static void triple_string_write(int node) {
  chunk_write("\"\"\"");
  int item;
  int start = 0;
  while (get_triple_string_item(node, &item, &start) == 0) {
    triple_string_item_write(item);
  }
  chunk_write("\"\"\"");
}

static int is_non_empty_normal_string(int node) {
  for (int i = 0; i < NUM_NORMAL_STRING_ITEM; ++i) {
    if (NORMAL_STRING[i] == (uint32_t)node) {
      return 1;
    }
  }
  return 0;
}

static int get_normal_string_item(int node, int *item, int *start) {
  for (int i = *start; i < NUM_NORMAL_STRING_ITEM; ++i) {
    if (NORMAL_STRING[i] == (uint32_t)node) {
      *item = NORMAL_STRING_ITEM[i];
      *start = i + 1;
      return 0;
    }
  }
  return -1;
}

static void in_parens_write(int node, int indent) {
  chunk_write("(");
  const int has_left = has_left_comment(node);
  const int left_is_multiline = has_multiline_left_comment(node);
  left_comments_with_spaces_write(0, node, indent + 1);
  expression_write(node, indent + 1);
  const int has_right = has_right_comment(node);
  const int right_is_multiline = has_multiline_right_comment(node);
  if (left_is_multiline && !has_right) {
    indent_write(indent);
  }
  if (!left_is_multiline && !has_right && is_multiline_node(node)) {
    indent_write(indent);
  }
  const int is_newline_right =
      has_right && ((has_left && left_is_multiline) || right_is_multiline);
  if (is_newline_right) {
    indent_write(indent + 1);
  }
  if (!has_left && has_right && !right_is_multiline) {
    char_write(' ');
  }
  right_comments_in_expression_write(node, indent + 1);
  if (is_newline_right) {
    indent_write(indent);
  }
  char_write(')');
}

static int get_in_parens(int node, int *expression) {
  for (int i = 0; i < NUM_IN_PARENS; ++i) {
    if (IN_PARENS_PARENT[i] == (uint32_t)node) {
      *expression = IN_PARENS[i];
      return 1;
    }
  }
  return 0;
}

static int get_record_update_name(int node, int *name) {
  for (int i = 0; i < NUM_RECORD_UPDATE_NAME; ++i) {
    if (RECORD_UPDATE[i] == (uint32_t)node) {
      *name = RECORD_UPDATE_NAME[i];
      return 1;
    }
  }
  return 0;
}

static void record_update_before_pipe_write(int node, int name, int indent) {
  chunk_write("{ ");
  left_comments_with_spaces_write(0, name, indent + 2);
  src_write(name);
  right_comments_with_spaces_write(name, indent + 2);
  const int has_right = has_right_comment(name);
  const int right_is_multiline = has_multiline_right_comment(name);
  if (has_right && !right_is_multiline) {
    char_write(' ');
  }
  if (has_right && right_is_multiline) {
    indent_write(floor_to_four(indent + 4));
  }
  if (!has_right && is_multiline_node(node)) {
    indent_write(floor_to_four(indent + 4));
  }
  if (!has_right && !is_multiline_node(node)) {
    char_write(' ');
  }
}

static void record_update_write(int node, int name, int indent) {
  record_update_before_pipe_write(node, name, indent);
  chunk_write("| ");
  int any_comment_is_multiline;
  const int is_multi = is_multiline_node(node);
  record_items_write(node, floor_to_four(indent + 4), &any_comment_is_multiline,
                     is_multi);
  if (any_comment_is_multiline || is_multi) {
    indent_write(indent);
  } else {
    char_write(' ');
  }
  char_write('}');
}

static int get_dot_function(int node, int *dot_function) {
  for (int i = 0; i < NUM_DOT_FUNCTION; ++i) {
    if (DOT_FUNCTION[i] == (uint32_t)node) {
      *dot_function = DOT_FUNCTION_NAME[i];
      return 1;
    }
  }
  return 0;
}

static void dotted_head_write(int dotted_head, int indent) {
  if (is_negative(dotted_head)) {
    chunk_write("-");
  }
  if (is_non_empty_record(dotted_head)) {
    non_empty_record_write(dotted_head, indent);
    return;
  }
  src_write(dotted_head);
}

static void dotted_write(int dotted_head, int dotted_tail, int indent) {
  dotted_head_write(dotted_head, indent);
  src_write(dotted_tail);
}

static int get_dotted(int dotted_head, int *dotted_tail) {
  for (int i = 0; i < NUM_DOTTED; ++i) {
    if (DOTTED_HEAD[i] == (uint32_t)dotted_head) {
      *dotted_tail = DOTTED_TAIL[i];
      return 1;
    }
  }
  return 0;
}

static int is_if_then_else_node(int node) {
  for (int i = 0; i < NUM_IF_THEN_ELSE; ++i) {
    if (IF_THEN_ELSE[i] == (uint32_t)node) {
      return 1;
    }
  }
  return 0;
}

static void right_comments_with_spaces_write(int node, int indent) {
  const int has_right = has_right_comment(node);
  const int has_multiline_right = has_multiline_right_comment(node);
  if (has_right && has_multiline_right) {
    indent_write(indent);
  }
  if (has_right && !has_multiline_right) {
    char_write(' ');
  }
  right_comments_in_expression_write(node, indent);
}

static void between_if_and_then_write(int is_in_else, int condition,
                                      int indent) {
  const int has_left = has_left_comment(condition);
  const int left_is_multiline = has_multiline_left_comment(condition);
  if (has_left && left_is_multiline) {
    indent_write(floor_to_four(indent + 4));
  }
  if (has_left && !left_is_multiline) {
    char_write(' ');
  }
  left_comments_with_spaces_write(0, condition, floor_to_four(indent + 4));
  const int is_multi_condition = is_multiline_node(condition) ||
                                 left_is_multiline || is_in_else ||
                                 has_multiline_right_comment(condition);
  if (is_multi_condition && !has_left) {
    indent_write(floor_to_four(indent + 4));
  }
  if (!is_multi_condition && !has_left) {
    char_write(' ');
  }
  expression_write(condition, floor_to_four(indent + 4));
  right_comments_with_spaces_write(condition, floor_to_four(indent + 4));
  if (is_multi_condition) {
    indent_write(indent);
  } else {
    char_write(' ');
  }
}

static void between_then_and_else_write(int then_branch, int indent) {
  indent_write(floor_to_four(indent + 4));
  left_comments_after_then_write(then_branch, floor_to_four(indent + 4));
  expression_write(then_branch, floor_to_four(indent + 4));
  const int has_right = has_right_comment(then_branch);
  if (has_right) {
    indent_write(floor_to_four(indent + 4));
  }
  right_comments_in_expression_write(then_branch, floor_to_four(indent + 4));
  char_write('\n');
  indent_write(indent);
}

static void left_comments_in_else_write(int node, int indent) {
  const int is_single = is_single_line_left_comments(node);
  const int is_if = is_if_then_else_node(node);
  const int new_indent = is_if ? indent : floor_to_four(indent + 4);
  int left_comment;
  int start = 0;
  if (get_left_comment(node, &left_comment, &start)) {
    return;
  }
  comment_write(left_comment, new_indent);
  while (get_left_comment(node, &left_comment, &start) == 0) {
    if (is_single) {
      char_write(' ');
    } else {
      indent_write(new_indent);
    }
    comment_write(left_comment, indent);
  }
}

static void whitespace_after_else_write(int else_branch, int indent) {
  const int has_left = has_left_comment(else_branch);
  const int left_is_multiline = has_multiline_left_comment(else_branch);
  const int is_if = is_if_then_else_node(else_branch);
  const int new_indent =
      is_if && left_is_multiline ? indent : floor_to_four(indent + 4);
  if (left_is_multiline || (has_left && !left_is_multiline && !is_if)) {
    indent_write(new_indent);
  }
  if (has_left && !left_is_multiline && is_if) {
    char_write(' ');
  }
  left_comments_in_else_write(else_branch, indent);
  if (is_if && !left_is_multiline) {
    char_write(' ');
    return;
  }
  indent_write(new_indent);
}

static void after_else_write(int else_branch, int indent) {
  whitespace_after_else_write(else_branch, indent);
  int condition_nested;
  int then_branch_nested;
  int else_branch_nested;
  const int is_if = get_if_then_else(else_branch, &condition_nested,
                                     &then_branch_nested, &else_branch_nested);
  const int expression_indent = is_if ? indent : indent + 4;
  const int has_multiline_left = has_multiline_left_comment(else_branch);
  if (is_if) {
    if_then_else_write(has_multiline_left, condition_nested, then_branch_nested,
                       else_branch_nested, indent);
    return;
  }
  expression_write(else_branch, expression_indent);
}

static void if_then_else_write(int is_in_else, int condition, int then_branch,
                               int else_branch, int indent) {
  chunk_write("if");
  between_if_and_then_write(is_in_else, condition, indent);
  chunk_write("then");
  between_then_and_else_write(then_branch, indent);
  chunk_write("else");
  after_else_write(else_branch, indent);
}

static int get_if_then_else(int node, int *condition, int *then_branch,
                            int *else_branch) {
  for (int i = 0; i < NUM_IF_THEN_ELSE; ++i) {
    if (IF_THEN_ELSE[i] == (uint32_t)node) {
      *condition = IF_THEN_ELSE_CONDITION[i];
      *then_branch = IF_THEN_ELSE_THEN[i];
      *else_branch = IF_THEN_ELSE_ELSE[i];
      return 1;
    }
  }
  return 0;
}

static int get_case_of_pivot(int node, int *pivot) {
  for (int i = 0; i < NUM_CASE_OF; ++i) {
    if (CASE_OF[i] == (uint32_t)node) {
      *pivot = CASE_OF_PIVOT[i];
      return 1;
    }
  }
  return 0;
}

static int get_case_of_branch(int node, int *left, int *right, int *start) {
  for (int i = *start; i < NUM_CASE_BRANCH; ++i) {
    if (CASE_OF_BRANCH_PARENT[i] == (uint32_t)node) {
      *left = CASE_OF_BRANCH_LEFT[i];
      *right = CASE_OF_BRANCH_RIGHT[i];
      *start = i + 1;
      return 1;
    }
  }
  return 0;
}

static void case_of_branch_write(int left, int right, int indent) {
  indent_write(floor_to_four(indent + 4));
  left_comments_with_spaces_write(1, left, floor_to_four(indent + 4));
  src_write(left);
  chunk_write(" ->");
  indent_write(floor_to_four(indent + 8));
  src_write(right);
}

static void between_case_and_of_write(int node, int pivot, int indent) {
  const int has_left = has_left_comment(pivot);
  const int left_is_multiline = has_multiline_left_comment(pivot);
  const int is_multiline = is_multiline_node(pivot) || is_multiline_node(node);
  if (has_left && left_is_multiline) {
    indent_write(floor_to_four(indent + 4));
  }
  if (has_left && !left_is_multiline) {
    char_write(' ');
  }
  if (!has_left && is_multiline) {
    indent_write(floor_to_four(indent + 4));
  }
  const int has_multiline_right = has_multiline_right_comment(pivot);
  if (!has_left && !is_multiline && has_multiline_right) {
    indent_write(floor_to_four(indent + 4));
  }
  left_comments_with_spaces_write(0, pivot, floor_to_four(indent + 4));
  if (is_multiline || left_is_multiline || has_multiline_right) {
    expression_write(pivot, floor_to_four(indent + 4));
    right_comments_with_spaces_write(pivot, floor_to_four(indent + 4));
    indent_write(indent);
    return;
  }
  if (!has_left) {
    char_write(' ');
  }
  expression_write(pivot, indent);
  right_comments_with_spaces_write(pivot, floor_to_four(indent + 4));
  char_write(' ');
}

static void case_of_write(int node, int pivot, int indent) {
  chunk_write("case");
  between_case_and_of_write(node, pivot, indent);
  chunk_write("of");
  int left;
  int right;
  int start = 0;
  if (get_case_of_branch(node, &left, &right, &start)) {
    case_of_branch_write(left, right, indent);
  }
  while (get_case_of_branch(node, &left, &right, &start)) {
    char_write('\n');
    case_of_branch_write(left, right, indent);
  }
}

static void not_infixed_write(int node, int indent) {
  int dotted_tail;
  if (get_dotted(node, &dotted_tail)) {
    dotted_write(node, dotted_tail, indent);
    return;
  }
  int if_condition;
  int if_then_branch;
  int if_else_branch;
  if (get_if_then_else(node, &if_condition, &if_then_branch, &if_else_branch)) {
    if_then_else_write(0, if_condition, if_then_branch, if_else_branch, indent);
    return;
  }
  int case_of_pivot;
  if (get_case_of_pivot(node, &case_of_pivot)) {
    case_of_write(node, case_of_pivot, indent);
    return;
  }
  if (is_hex(node)) {
    hex_write(node);
    return;
  }
  if (is_exponent_float(node)) {
    exponent_float_write(node);
    return;
  }
  if (is_empty_list(node)) {
    chunk_write("[]");
    return;
  }
  if (is_empty_tuple(node)) {
    chunk_write("()");
    return;
  }
  if (is_empty_record(node)) {
    chunk_write("{}");
    return;
  }
  if (is_non_empty_list(node)) {
    non_empty_list_write(node, indent);
    return;
  }
  int update_name;
  if (get_record_update_name(node, &update_name)) {
    record_update_write(node, update_name, indent);
    return;
  }
  if (is_non_empty_record(node)) {
    non_empty_record_write(node, indent);
    return;
  }
  if (is_non_empty_tuple(node)) {
    non_empty_tuple_write(node, indent);
    return;
  }
  if (has_arguments(node)) {
    function_call_write(node, indent);
    return;
  }
  int dot_function;
  if (get_dot_function(node, &dot_function)) {
    char_write('.');
    src_write(dot_function);
    return;
  }
  if (is_empty_triple_string(node)) {
    chunk_write("\"\"\"\"\"\"");
    return;
  }
  if (is_non_empty_triple_string(node)) {
    triple_string_write(node);
    return;
  }
  if (is_empty_normal_string(node)) {
    chunk_write("\"\"");
    return;
  }
  if (is_non_empty_normal_string(node)) {
    normal_string_write(node);
    return;
  }
  int expression;
  if (get_in_parens(node, &expression)) {
    in_parens_write(expression, indent);
    return;
  }
  src_write(node);
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
  I = 0;
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
  NUM_SRC_MULTILINE = 0;
  NUM_SAME_LINE_COMMENT = 0;
  NUM_TITLE_COMMENT = 0;
  NUM_DOUBLE_HYPHEN_BLOCK = 0;
  NUM_ARGUMENT = 0;
  NUM_IS_ARG1_LINE1 = 0;
  NUM_PLUS = 0;
  NUM_RIGHT_COMMENT = 0;
  NUM_MINUS = 0;
  NUM_ASTERISK = 0;
  NUM_DIVIDE = 0;
  NUM_POWER = 0;
  NUM_GREATER_THAN = 0;
  NUM_LESS_THAN = 0;
  NUM_INT_DIVIDE = 0;
  NUM_OUT = 0;
  NUM_TRIPLE_STRING_ITEM = 0;
  NUM_EMPTY_TRIPLE_STRING = 0;
  NUM_EMPTY_NORMAL_STRING = 0;
  NUM_NORMAL_STRING_ITEM = 0;
  NUM_PLUS_PLUS = 0;
  NUM_GREATER_THAN_OR_EQUAL = 0;
  NUM_LESS_THAN_OR_EQUAL = 0;
  NUM_EQUAL = 0;
  NUM_OR = 0;
  NUM_AND = 0;
  NUM_NOT_EQUAL = 0;
  NUM_PARSE_DOT = 0;
  NUM_PARSE_EQUALS = 0;
  NUM_COMPOSE_LEFT = 0;
  NUM_COMPOSE_RIGHT = 0;
  NUM_LEFT_PIZZA = 0;
  NUM_MULTILINE_INFIX = 0;
  NUM_RIGHT_PIZZA = 0;
  NUM_IN_PARENS = 0;
  NUM_TUPLE_ITEM = 0;
  NUM_EMPTY_TUPLE = 0;
  NUM_EMPTY_RECORD = 0;
  NUM_CONS = 0;
  NUM_RECORD_FIELD = 0;
  NUM_RECORD_ITEM = 0;
  NUM_RECORD_UPDATE_NAME = 0;
  NUM_DOT_FUNCTION = 0;
  NUM_DOTTED = 0;
  NUM_IF_THEN_ELSE = 0;
  NUM_CASE_OF = 0;
  NUM_CASE_BRANCH = 0;
}

static int get_new_node() {
  if (NUM_NODES == MAX_NODES) {
    fprintf(stderr, "%s: too many nodes, maximum is %d\n", PATH, MAX_NODES);
    exit(-1);
  }
  ++NUM_NODES;
  return NUM_NODES - 1;
}

static int is_digit(uint8_t c) { return c >= '0' && c <= '9'; }

static int any_char_parse(uint8_t *c) {
  if (I == NUM_SRC) {
    return -1;
  }
  *c = SRC[I];
  ++I;
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
  if (I == NUM_SRC) {
    return -1;
  }
  if (SRC[I] != c) {
    return -1;
  }
  ++I;
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
  append_has_src(*node, start, I - start);
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
  if (digit_parse()) {
    I = start;
    return -1;
  }
  while (digit_parse() == 0) {
  }
  int end = I;
  while (backwards_char_parse('0', &end) == 0) {
  }
  if (SRC[end - 1] == '.') {
    ++end;
  }
  *node = get_new_node();
  append_has_src(*node, start, end - start);
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
  append_has_src(exp_node, start_exp, I - start_exp);
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
  append_has_src(*node, start, I - start);
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

static int normal_string_not_hex_item_item_parse() {
  const int start = I;
  if (chunk_parse("\\u{") == 0) {
    I = start;
    return -1;
  }
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
  return chunk_parse("\\t");
}

static int normal_string_not_hex_item_parse(int *node) {
  const int start = I;
  if (normal_string_not_hex_item_item_parse()) {
    return -1;
  }
  while (normal_string_not_hex_item_item_parse() == 0) {
  }
  *node = get_new_node();
  append_has_src(*node, start, I - start);
  return 0;
}

static int normal_string_item_parse(int *node) {
  if (normal_string_not_hex_item_parse(node) == 0) {
    return 0;
  }
  return string_hex_parse(node);
}

static int triple_string_not_hex_item_item_parse() {
  const int start = I;
  if (chunk_parse("\"\"\"") == 0) {
    I = start;
    return -1;
  }
  if (chunk_parse("\\u{") == 0) {
    I = start;
    return -1;
  }
  if (chunk_parse("\\\"") == 0) {
    return 0;
  }
  if (chunk_parse("\\\\") == 0) {
    return 0;
  }
  uint8_t dont_care;
  if (any_char_parse(&dont_care)) {
    I = start;
    return -1;
  }
  return 0;
}

static int triple_string_not_hex_item_parse(int *node) {
  const int start = I;
  if (triple_string_not_hex_item_item_parse()) {
    return -1;
  }
  while (triple_string_not_hex_item_item_parse() == 0) {
  }
  *node = get_new_node();
  append_has_src(*node, start, I - start);
  return 0;
}

static int string_hex_parse(int *node) {
  const int start = I;
  if (chunk_parse("\\u{")) {
    return -1;
  }
  const int hex_start = I;
  while (hex_char_parse() == 0) {
  }
  const int hex_end = I;
  if (char_parse('}') != 0) {
    I = start;
    return -1;
  }
  *node = get_new_node();
  append_has_src(*node, hex_start, hex_end - hex_start);
  append_is_hex(*node);
  return 0;
}

static int triple_string_item_parse(int *node) {
  if (triple_string_not_hex_item_parse(node) == 0) {
    return 0;
  }
  return string_hex_parse(node);
}

static int non_empty_normal_string_parse(int *node) {
  const int start = I;
  if (char_parse('"')) {
    return -1;
  }
  int item;
  *node = get_new_node();
  while (normal_string_item_parse(&item) == 0) {
    append_normal_string_item(*node, item);
  }
  if (char_parse('"')) {
    I = start;
    return -1;
  }
  return 0;
}

static int empty_normal_string_parse(int *node) {
  if (chunk_parse("\"\"")) {
    return -1;
  }
  *node = get_new_node();
  append_empty_normal_string(*node);
  return 0;
}

static int normal_string_parse(int *node) {
  if (empty_normal_string_parse(node) == 0) {
    return 0;
  }
  return non_empty_normal_string_parse(node);
}

static int non_empty_triple_string_parse(int *node) {
  const int start = I;
  if (chunk_parse("\"\"\"")) {
    return -1;
  }
  int item;
  *node = get_new_node();
  while (triple_string_item_parse(&item) == 0) {
    append_triple_string_item(*node, item);
  }
  if (chunk_parse("\"\"\"")) {
    I = start;
    return -1;
  }
  return 0;
}

static int empty_triple_string_parse(int *node) {
  if (chunk_parse("\"\"\"\"\"\"")) {
    return -1;
  }
  *node = get_new_node();
  append_empty_triple_string(*node);
  return 0;
}

static int triple_string_parse(int *node) {
  if (empty_triple_string_parse(node) == 0) {
    return 0;
  }
  return non_empty_triple_string_parse(node);
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

static int any_keyword_parse() {
  if (keyword_parse("if") == 0) {
    return 0;
  }
  if (keyword_parse("then") == 0) {
    return 0;
  }
  if (keyword_parse("else") == 0) {
    return 0;
  }
  if (keyword_parse("case") == 0) {
    return 0;
  }
  if (keyword_parse("of") == 0) {
    return 0;
  }
  return -1;
}

static int lower_name_parse(int *node) {
  const int start = I;
  if (any_keyword_parse() == 0) {
    I = start;
    return -1;
  }
  if (first_lower_name_char_parse()) {
    return -1;
  }

  while (subsequent_name_char_parse() == 0) {
  }

  *node = get_new_node();
  append_has_src(*node, start, I - start);
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
  append_has_src(*node, start, I - start);
  return 0;
}

static void whitespace_parse() {
  while (char_parse(' ') == 0 || char_parse('\n') == 0) {
  }
}

static void spaces_parse() {
  while (char_parse(' ') == 0) {
  }
}

static int empty_list_parse(int *node) {
  const int start = I;
  if (char_parse('[')) {
    return -1;
  }
  whitespace_parse();
  if (char_parse(']')) {
    I = start;
    return -1;
  }
  *node = get_new_node();
  append_is_empty_list(*node);
  return 0;
}

static int right_comments_with_title_parse() {
  const int parent = get_new_node();
  spaces_parse();
  int same_line_comment;
  if (line_comment_parse(&same_line_comment) == 0) {
    append_same_line_comment(parent, same_line_comment);
  }
  whitespace_parse();
  title_comments_parse(parent);
  whitespace_parse();
  return parent;
}

static int list_item_parse(int *node) {
  const int start = I;
  whitespace_parse();
  const int left_comment = left_comments_parse();
  whitespace_parse();
  if (expression_parse(node)) {
    I = start;
    return -1;
  }
  attach_left_comment(*node, left_comment);
  const int right_comment = right_comments_with_title_parse();
  attach_right_comment(*node, right_comment);
  return 0;
}

static int tuple_item_parse(int *node) {
  const int start = I;
  whitespace_parse();
  const int left_comment = left_comments_parse();
  whitespace_parse();
  if (expression_parse(node)) {
    I = start;
    return -1;
  }
  whitespace_parse();
  attach_left_comment(*node, left_comment);
  const int right_comment = right_comments_in_expression_parse();
  attach_right_comment_in_expression(*node, right_comment);
  return 0;
}

static int record_item_name_parse(int *name, int *start_row) {
  const int start = I;
  whitespace_parse();
  *start_row = ROW[I];
  if (lower_name_parse(name)) {
    I = start;
    return -1;
  }
  whitespace_parse();
  const int comments_after_name = right_comments_in_expression_parse();
  attach_right_comment_in_expression(*name, comments_after_name);
  return 0;
}

static int record_item_value_parse(int *value, int *end_row) {
  const int left_comment_on_value = left_comments_parse();
  const int start = I;
  whitespace_parse();
  if (expression_parse(value)) {
    I = start;
    return -1;
  }
  *end_row = ROW[I];
  const int right_comment = right_comments_with_title_parse();
  attach_left_comment(*value, left_comment_on_value);
  attach_right_comment(*value, right_comment);
  return 0;
}

static int record_item_parse(int *node) {
  const int start = I;
  whitespace_parse();
  *node = get_new_node();
  int name;
  int start_row;
  const int left_comment = left_comments_parse();
  attach_left_comment(*node, left_comment);
  if (record_item_name_parse(&name, &start_row)) {
    I = start;
    return -1;
  }
  whitespace_parse();
  if (char_parse('=')) {
    I = start;
    return -1;
  }
  whitespace_parse();
  int value;
  int end_row;
  record_item_value_parse(&value, &end_row);
  if (end_row > start_row) {
    append_src_multiline(*node);
  }
  whitespace_parse();
  append_record_field(*node, name, value);
  return 0;
}

static int record_items_parse(int *node, int start, int start_row) {
  int item;
  if (record_item_parse(&item)) {
    I = start;
    return -1;
  }
  *node = get_new_node();
  append_record_item(*node, item);
  while (1) {
    if (char_parse(',')) {
      break;
    }
    if (record_item_parse(&item)) {
      I = start;
      return -1;
    }
    append_record_item(*node, item);
  }
  char_parse(' ');
  if (char_parse('}')) {
    I = start;
    return -1;
  }
  if (ROW[I] > start_row) {
    append_src_multiline(*node);
  }
  return 0;
}

static int record_update_parse(int *node) {
  const int start = I;
  const int start_row = ROW[I];
  if (char_parse('{')) {
    return -1;
  }
  whitespace_parse();
  const int left_comment = left_comments_parse();
  whitespace_parse();
  int name;
  if (lower_name_parse(&name)) {
    I = start;
    return -1;
  }
  attach_left_comment(name, left_comment);
  whitespace_parse();
  const int right_comment = right_comments_in_expression_parse();
  attach_right_comment_in_expression(name, right_comment);
  whitespace_parse();
  if (char_parse('|')) {
    I = start;
    return -1;
  }
  *node = get_new_node();
  if (record_items_parse(node, start, start_row)) {
    I = start;
    return -1;
  }
  append_record_update_name(*node, name);
  return 0;
}

static int non_empty_record_parse(int *node) {
  const int start = I;
  const int start_row = ROW[I];
  if (char_parse('{')) {
    return -1;
  }
  return record_items_parse(node, start, start_row);
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
  if (ROW[I] - start_row) {
    append_src_multiline(*node);
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
  const int start = I;
  whitespace_parse();
  const int left_comment = left_comments_parse();
  if (argument_parse(argument)) {
    I = start;
    return -1;
  }
  attach_left_comment(*argument, left_comment);
  return 0;
}

static int callable_parse(int *node) {
  if (upper_name_parse(node) == 0) {
    return 0;
  }
  if (lower_name_parse(node) == 0) {
    return 0;
  }
  if (dot_function_parse(node) == 0) {
    return 0;
  }
  return -1;
}

static int after_callable_parse() {
  if (char_parse('(') == 0) {
    --I;
    return 0;
  }
  if (char_parse('[') == 0) {
    --I;
    return 0;
  }
  if (char_parse('{') == 0) {
    --I;
    return 0;
  }
  if (chunk_parse("--") == 0) {
    I -= 2;
    return 0;
  }
  if (char_parse(' ') == 0) {
    return 0;
  }
  if (char_parse('\n') == 0) {
    return 0;
  }
  return -1;
}

static int function_call_parse(int *node) {
  const int start = I;
  const int start_row = ROW[I];
  if (callable_parse(node)) {
    return -1;
  }
  if (after_callable_parse()) {
    I = start;
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
    append_is_arg1_line1(argument);
  }
  while (1) {
    if (argument_and_comments_parse(&argument)) {
      break;
    }
    append_argument(*node, argument);
  }
  if (ROW[I] > start_row) {
    append_src_multiline(*node);
  }
  return 0;
}

static int in_necessary_parens_parse(int *node) {
  const int start = I;
  if (char_parse('(')) {
    return -1;
  }
  whitespace_parse();
  int expression;
  if (function_call_parse(&expression) && infixed_parse(&expression) &&
      if_then_else_parse(&expression)) {
    I = start;
    return -1;
  }
  whitespace_parse();
  if (char_parse(')')) {
    I = start;
    return -1;
  }
  *node = get_new_node();
  append_in_parens(*node, expression);
  return 0;
}

static int argument_parse(int *node) {
  if (simple_expression_parse(node) == 0) {
    return 0;
  }
  if (argument_in_unnecessary_parens_parse(node) == 0) {
    return 0;
  }
  return in_necessary_parens_parse(node);
}

static void attach_same_line_comment(int node, int same_line_comment) {
  for (int i = 0; i < NUM_SAME_LINE_COMMENT; ++i) {
    if (SAME_LINE_COMMENT_PARENT[i] == (uint32_t)same_line_comment) {
      SAME_LINE_COMMENT_PARENT[i] = node;
    }
  }
}

static void attach_title_comments(int node, int title_comments) {
  for (int i = 0; i < NUM_TITLE_COMMENT; ++i) {
    if (TITLE_COMMENT_PARENT[i] == (uint32_t)title_comments) {
      TITLE_COMMENT_PARENT[i] = node;
    }
  }
}

static void attach_right_comment(int node, int right_comment) {
  attach_same_line_comment(node, right_comment);
  attach_title_comments(node, right_comment);
}

static int infix_then_expression_parse(int *node, char *infix) {
  const int start = I;
  whitespace_parse();
  const int left_comment = left_comments_parse();
  whitespace_parse();
  if (chunk_parse(infix)) {
    I = start;
    return -1;
  }
  whitespace_parse();
  const int right_comment = right_comments_in_expression_parse();
  whitespace_parse();
  if (infixed_item_parse(node)) {
    I = start;
    return -1;
  }
  attach_left_comment(*node, left_comment);
  attach_right_comment_in_expression(*node, right_comment);
  return 0;
}

static int infix_parse(int *left) {
  int right;
  if (infix_then_expression_parse(&right, "++") == 0) {
    append_plus_plus(*left, right);
    *left = right;
    return 0;
  }
  if (infix_then_expression_parse(&right, "::") == 0) {
    append_cons(*left, right);
    *left = right;
    return 0;
  }
  if (infix_then_expression_parse(&right, "+") == 0) {
    append_plus(*left, right);
    *left = right;
    return 0;
  }
  if (infix_then_expression_parse(&right, "-") == 0) {
    append_minus(*left, right);
    *left = right;
    return 0;
  }
  if (infix_then_expression_parse(&right, "*") == 0) {
    append_asterisk(*left, right);
    *left = right;
    return 0;
  }
  if (infix_then_expression_parse(&right, "//") == 0) {
    append_int_divide(*left, right);
    *left = right;
    return 0;
  }
  if (infix_then_expression_parse(&right, "/") == 0) {
    append_divide(*left, right);
    *left = right;
    return 0;
  }
  if (infix_then_expression_parse(&right, "^") == 0) {
    append_power(*left, right);
    *left = right;
    return 0;
  }
  if (infix_then_expression_parse(&right, ">=") == 0) {
    append_greater_than_or_equal(*left, right);
    *left = right;
    return 0;
  }
  if (infix_then_expression_parse(&right, ">") == 0) {
    append_greater_than(*left, right);
    *left = right;
    return 0;
  }
  if (infix_then_expression_parse(&right, "<=") == 0) {
    append_less_than_or_equal(*left, right);
    *left = right;
    return 0;
  }
  if (infix_then_expression_parse(&right, "<") == 0) {
    append_less_than(*left, right);
    *left = right;
    return 0;
  }
  if (infix_then_expression_parse(&right, "==") == 0) {
    append_equal(*left, right);
    *left = right;
    return 0;
  }
  if (infix_then_expression_parse(&right, "||") == 0) {
    append_or(*left, right);
    *left = right;
    return 0;
  }
  if (infix_then_expression_parse(&right, "&&") == 0) {
    append_and(*left, right);
    *left = right;
    return 0;
  }
  if (infix_then_expression_parse(&right, "/=") == 0) {
    append_not_equal(*left, right);
    *left = right;
    return 0;
  }
  if (infix_then_expression_parse(&right, "|.") == 0) {
    append_parse_dot(*left, right);
    *left = right;
    return 0;
  }
  if (infix_then_expression_parse(&right, "|=") == 0) {
    append_parse_equals(*left, right);
    *left = right;
    return 0;
  }
  if (infix_then_expression_parse(&right, "<<") == 0) {
    append_compose_left(*left, right);
    *left = right;
    return 0;
  }
  if (infix_then_expression_parse(&right, ">>") == 0) {
    append_compose_right(*left, right);
    *left = right;
    return 0;
  }
  if (infix_then_expression_parse(&right, "<|") == 0) {
    append_left_pizza(*left, right);
    *left = right;
    return 0;
  }
  if (infix_then_expression_parse(&right, "|>") == 0) {
    append_right_pizza(*left, right);
    *left = right;
    return 0;
  }
  return -1;
}

static int infixed_parse(int *node) {
  const int start = I;
  const int start_row = ROW[I];
  if (not_infixed_parse(node)) {
    I = start;
    return -1;
  }
  int left = *node;
  if (infix_parse(&left)) {
    I = start;
    return -1;
  }
  while (infix_parse(&left) == 0) {
  }
  if (ROW[I] > start_row) {
    append_multiline_infix(*node);
  }
  return 0;
}

static int expression_parse(int *node) {
  if (infixed_parse(node) == 0) {
    return 0;
  }
  return not_infixed_parse(node);
}

static int qualified_name_part_parse() {
  const int start = I;
  if (char_parse('.')) {
    I = start;
    return -1;
  }
  int dont_care;
  if (lower_name_parse(&dont_care) && upper_name_parse(&dont_care)) {
    I = start;
    return -1;
  }
  return 0;
}

static int dottable_name_part_parse() {
  const int start = I;
  if (char_parse('.')) {
    I = start;
    return -1;
  }
  int dont_care;
  if (lower_name_parse(&dont_care)) {
    I = start;
    return -1;
  }
  return 0;
}

static int dottable_parse(int *node) {
  if (lower_name_parse(node) == 0) {
    return 0;
  }
  if (record_parse(node) == 0) {
    return 0;
  }
  return -1;
}

static int dotted_parse(int *node) {
  const int start = I;
  const int is_negative = char_parse('-') == 0;
  if (dottable_parse(node)) {
    I = start;
    return -1;
  }
  if (is_negative) {
    append_is_negative(*node);
  }
  const int tail_start = I;
  if (dottable_name_part_parse()) {
    I = start;
    return -1;
  }
  while (dottable_name_part_parse() == 0) {
  }
  int tail = get_new_node();
  append_has_src(tail, tail_start, I - tail_start);
  append_dotted(*node, tail);
  return 0;
}

static int qualified_name_parse(int *node) {
  const int start = I;
  int dont_care;
  if (upper_name_parse(&dont_care)) {
    return -1;
  }
  if (qualified_name_part_parse()) {
    I = start;
    return -1;
  }
  while (qualified_name_part_parse() == 0) {
  }
  *node = get_new_node();
  append_has_src(*node, start, I - start);
  return 0;
}

static int argument_in_unnecessary_parens_contents_parse(int *node) {
  if (argument_in_unnecessary_parens_parse(node) == 0) {
    return 0;
  }
  return simple_expression_parse(node);
}

static int in_unnecessary_parens_contents_parse(int *node) {
  if (function_call_parse(node) == 0) {
    return 0;
  }
  if (infixed_parse(node) == 0) {
    return 0;
  }
  if (in_unnecessary_parens_parse(node) == 0) {
    return 0;
  }
  if (if_then_else_parse(node) == 0) {
    return 0;
  }
  return simple_expression_parse(node);
}

static int argument_in_unnecessary_parens_parse(int *node) {
  const int start = I;
  if (char_parse('(')) {
    return -1;
  }
  whitespace_parse();
  if (argument_in_unnecessary_parens_contents_parse(node)) {
    I = start;
    return -1;
  }
  whitespace_parse();
  if (char_parse(')')) {
    I = start;
    return -1;
  }
  return 0;
}

static int in_unnecessary_parens_parse(int *node) {
  const int start = I;
  if (char_parse('(')) {
    return -1;
  }
  whitespace_parse();
  if (in_unnecessary_parens_contents_parse(node)) {
    I = start;
    return -1;
  }
  whitespace_parse();
  if (char_parse(')')) {
    I = start;
    return -1;
  }
  return 0;
}

static int pattern_in_unnecessary_parens_contents_parse(int *node) {
  if (lower_name_parse(node) == 0) {
    return 0;
  }
  return 0;
}

static int pattern_in_unnecessary_parens_parse(int *node) {
  const int start = I;
  if (char_parse('(')) {
    return -1;
  }
  whitespace_parse();
  if (pattern_in_unnecessary_parens_contents_parse(node)) {
    I = start;
    return -1;
  }
  whitespace_parse();
  if (char_parse(')')) {
    I = start;
    return -1;
  }
  return 0;
}

static int with_comments_in_parentheses_parse(int *node) {
  const int start = I;
  if (char_parse('(')) {
    return -1;
  }
  whitespace_parse();
  const int left_comment = left_comments_parse();
  whitespace_parse();
  int expression;
  if (expression_parse(&expression)) {
    I = start;
    return -1;
  }
  whitespace_parse();
  const int right_comment = right_comments_in_expression_parse();
  if (char_parse(')')) {
    I = start;
    return -1;
  }
  *node = get_new_node();
  const int num_comments =
      num_left_comments(left_comment) + num_right_comments(right_comment);
  if (num_comments == 0) {
    I = start;
    return -1;
  }
  append_in_parens(*node, expression);
  attach_left_comment(expression, left_comment);
  attach_right_comment_in_expression(expression, right_comment);
  return 0;
}

static int non_empty_tuple_parse(int *node) {
  const int start = I;
  const int start_row = ROW[I];
  if (char_parse('(')) {
    return -1;
  }
  int item;
  if (tuple_item_parse(&item)) {
    I = start;
    return -1;
  }
  *node = get_new_node();
  append_tuple_item(*node, item);
  if (char_parse(',')) {
    I = start;
    return -1;
  }
  if (tuple_item_parse(&item)) {
    I = start;
    return -1;
  }
  append_tuple_item(*node, item);
  while (1) {
    if (char_parse(',')) {
      break;
    }
    if (tuple_item_parse(&item)) {
      I = start;
      return -1;
    }
    append_tuple_item(*node, item);
  }
  if (char_parse(')')) {
    I = start;
    return -1;
  }
  if (ROW[I] - start_row) {
    append_src_multiline(*node);
  }
  return 0;
}

static int empty_tuple_parse(int *node) {
  const int start = I;
  if (char_parse('(')) {
    return -1;
  }
  whitespace_parse();
  if (char_parse(')')) {
    I = start;
    return -1;
  }
  *node = get_new_node();
  append_is_empty_tuple(*node);
  return 0;
}

static int empty_record_parse(int *node) {
  const int start = I;
  if (char_parse('{')) {
    return -1;
  }
  whitespace_parse();
  if (char_parse('}')) {
    I = start;
    return -1;
  }
  *node = get_new_node();
  append_is_empty_record(*node);
  return 0;
}

static int record_parse(int *node) {
  if (empty_record_parse(node) == 0) {
    return 0;
  }
  if (non_empty_record_parse(node) == 0) {
    return 0;
  }
  return record_update_parse(node);
}

static int tuple_parse(int *node) {
  if (non_empty_tuple_parse(node) == 0) {
    return 0;
  }
  return empty_tuple_parse(node);
}

static int dot_function_parse(int *node) {
  const int start = I;
  if (char_parse('.')) {
    return -1;
  }
  int name;
  if (lower_name_parse(&name)) {
    I = start;
    return -1;
  }
  *node = get_new_node();
  append_dot_function(*node, name);
  return 0;
}

static int negative_lower_name_parse(int *node) {
  const int start = I;
  if (char_parse('-')) {
    return -1;
  }
  int dont_care;
  if (lower_name_parse(&dont_care)) {
    I = start;
    return -1;
  }
  *node = get_new_node();
  append_has_src(*node, start, I - start);
  return 0;
}

static int simple_expression_parse(int *node) {
  if (dotted_parse(node) == 0) {
    return 0;
  }
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
  if (qualified_name_parse(node) == 0) {
    return 0;
  }
  if (upper_name_parse(node) == 0) {
    return 0;
  }
  if (negative_lower_name_parse(node) == 0) {
    return 0;
  }
  if (lower_name_parse(node) == 0) {
    return 0;
  }
  if (dot_function_parse(node) == 0) {
    return 0;
  }
  if (with_comments_in_parentheses_parse(node) == 0) {
    return 0;
  }
  if (tuple_parse(node) == 0) {
    return 0;
  }
  if (record_parse(node) == 0) {
    return 0;
  }
  return list_parse(node);
}

static int infixed_item_parse(int *node) {
  if (function_call_parse(node) == 0) {
    return 0;
  }
  if (simple_expression_parse(node) == 0) {
    return 0;
  }
  if (if_then_else_parse(node) == 0) {
    return 0;
  }
  if (in_necessary_parens_parse(node) == 0) {
    return 0;
  }
  return argument_in_unnecessary_parens_parse(node);
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

static int is_after_keyword_char(uint8_t c) {
  return c == ' ' || c == '\n' || c == '(' || c == '[' || c == '{' ||
         c == '}' || c == ']' || c == ')' || c == '-';
}

static int after_keyword_char_parse() {
  const int start = I;
  uint8_t ch;
  if (any_char_parse(&ch)) {
    return -1;
  }
  if (!is_after_keyword_char(ch)) {
    I = start;
    return -1;
  }
  --I;
  return 0;
}

static int keyword_parse(char *keyword) {
  const int start = I;
  if (chunk_parse(keyword)) {
    return -1;
  }
  if (after_keyword_char_parse()) {
    I = start;
    return -1;
  }
  return 0;
}

static int if_then_else_part_parse(int *node) {
  whitespace_parse();
  const int left_comment = left_comments_parse();
  whitespace_parse();
  if (expression_parse(node)) {
    return -1;
  }
  whitespace_parse();
  const int right_comment = right_comments_in_expression_parse();
  attach_right_comment_in_expression(*node, right_comment);
  attach_left_comment(*node, left_comment);
  whitespace_parse();
  return 0;
}

static int if_then_else_helper_parse(int *node) {
  if (keyword_parse("if")) {
    return -1;
  }
  int condition;
  if (if_then_else_part_parse(&condition)) {
    return -1;
  }
  if (keyword_parse("then")) {
    return -1;
  }
  whitespace_parse();
  int then_branch;
  if (if_then_else_part_parse(&then_branch)) {
    return -1;
  }
  if (keyword_parse("else")) {
    return -1;
  }
  whitespace_parse();
  int else_branch;
  if (if_then_else_part_parse(&else_branch)) {
    return -1;
  }
  *node = get_new_node();
  append_if_then_else(*node, condition, then_branch, else_branch);
  return 0;
}

static int if_then_else_parse(int *node) {
  const int start = I;
  if (if_then_else_helper_parse(node)) {
    I = start;
    return -1;
  }
  return 0;
}

static int case_of_pivot_parse(int *node) {
  const int start = I;
  const int start_row = ROW[I];
  if (keyword_parse("case")) {
    return -1;
  }
  whitespace_parse();
  const int left_comment = left_comments_parse();
  int expression;
  if (expression_parse(&expression)) {
    I = start;
    return -1;
  }
  attach_left_comment(expression, left_comment);
  whitespace_parse();
  const int right_comment = right_comments_in_expression_parse();
  attach_right_comment_in_expression(expression, right_comment);
  whitespace_parse();
  if (keyword_parse("of")) {
    I = start;
    return -1;
  }
  *node = get_new_node();
  if (ROW[I] > start_row) {
    append_src_multiline(*node);
  }
  append_case_of(*node, expression);
  return 0;
}

static int wildcard_parse(int *node) {
  const int start = I;
  if (char_parse('_')) {
    return -1;
  }
  *node = get_new_node();
  append_has_src(*node, start, 1);
  return 0;
}

static int pattern_parse(int *node) {
  if (lower_name_parse(node) == 0) {
    return 0;
  }
  if (wildcard_parse(node) == 0) {
    return 0;
  }
  if (pattern_in_unnecessary_parens_parse(node) == 0) {
    return 0;
  }
  return upper_name_parse(node);
}

static int case_of_branch_parse(int *node) {
  const int start = I;
  whitespace_parse();
  const int left_comment = left_comments_parse();
  int pattern;
  if (pattern_parse(&pattern)) {
    I = start;
    return -1;
  }
  attach_left_comment(pattern, left_comment);
  char_parse(' ');
  if (chunk_parse("->")) {
    I = start;
    return -1;
  }
  whitespace_parse();
  int result;
  if (lower_name_parse(&result)) {
    I = start;
    return -1;
  }
  *node = get_new_node();
  append_case_branch(*node, pattern, result);
  return 0;
}

static int case_of_parse(int *node) {
  if (case_of_pivot_parse(node)) {
    return -1;
  }
  int branch;
  while (case_of_branch_parse(&branch) == 0) {
    attach_case_of_branch(*node, branch);
  }
  return 0;
}

static int not_infixed_parse(int *node) {
  if (function_call_parse(node) == 0) {
    return 0;
  }
  if (simple_expression_parse(node) == 0) {
    return 0;
  }
  if (if_then_else_parse(node) == 0) {
    return 0;
  }
  if (case_of_parse(node) == 0) {
    return 0;
  }
  return in_unnecessary_parens_parse(node);
}

static int line_comment_parse(int *node) {
  const int start = I;
  if (chunk_parse("--")) {
    return -1;
  }
  while (not_newline_parse() == 0) {
  }
  while (backwards_char_parse(' ', &I) == 0) {
  }
  const int end = I;
  spaces_parse();
  *node = get_new_node();
  append_has_src(*node, start, end - start);
  return 0;
}

static int empty_block_comment_parse(int *node) {
  const int start = I;
  *node = get_new_node();
  if (chunk_parse("{-")) {
    return -1;
  }
  spaces_parse();
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

static int backwards_char_parse(uint8_t c, int *i) {
  if (*i > 2 && SRC[*i - 1] == c) {
    --*i;
    return 0;
  }
  return -1;
}

static void block_comment_line_parse(uint32_t *start, uint16_t *size,
                                     int *nesting) {
  spaces_parse();
  *start = I;
  while (block_comment_item_parse(nesting) == 0) {
  }
  while (backwards_char_parse(' ', &I) == 0) {
  }
  *size = I - *start;
  spaces_parse();
  char_parse('\n');
}

static int is_block_comment_end() {
  const int start = I;
  spaces_parse();
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
  spaces_parse();
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
  spaces_parse();
  const int start = I;
  uint8_t dont_care;
  while (chunk_parse("-}") && any_char_parse(&dont_care) == 0) {
  }
  I -= 2;
  while (backwards_char_parse(' ', &I) == 0) {
  }
  const int end = I;
  spaces_parse();
  if (chunk_parse("-}")) {
    I = start;
    return -1;
  }
  *node = get_new_node();
  append_has_src(*node, start, end - start);
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

static int left_comments_parse() {
  const int parent = get_new_node();
  for (int comment; comment_parse(&comment) == 0;
       append_left_comment(parent, comment)) {
    whitespace_parse();
  }
  return parent;
}

static int right_comments_in_expression_parse() {
  const int node = get_new_node();
  for (int comment; comment_parse(&comment) == 0;
       append_right_comment(node, comment)) {
    whitespace_parse();
  }
  return node;
}

static void title_comments_parse(int parent) {
  for (int comment; comment_parse(&comment) == 0;
       append_title_comment(parent, comment)) {
    whitespace_parse();
  }
}

static void attach_left_comment(int node, int left_comment) {
  for (int i = 0; i < NUM_LEFT_COMMENT; ++i) {
    if (LEFT_COMMENT_PARENT[i] == (uint32_t)left_comment) {
      LEFT_COMMENT_PARENT[i] = (uint32_t)node;
    }
  }
}

static int module_parse(int *node) {
  if (chunk_parse("module X exposing (x)\n\n\nx =\n    ")) {
    return -1;
  }
  const int left_comment = left_comments_parse();
  spaces_parse();
  if (expression_parse(node)) {
    return -1;
  }
  attach_left_comment(*node, left_comment);
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
    char_write(' ');
  }
  if (i > 0) {
    char_write('\n');
  }
  if (size > 0 && i > 0) {
    for (int j = 0; j < indent; ++j) {
      char_write(' ');
    }
    chunk_write("   ");
  }
  out_write(SRC + start, size);
}

static void non_empty_block_comment_write(int node, int indent) {
  chunk_write("{-");
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
    char_write(' ');
  }
  chunk_write("-}");
}

static void double_hypen_block_comment_write(int node) {
  chunk_write("{--");
  src_write(node);
  chunk_write("-}");
}

static void comment_write(int node, int indent) {
  if (is_empty_block_comment(node)) {
    chunk_write("{--}");
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

static int is_single_line_left_comments(int node) {
  int left_comment;
  int start = 0;
  while (get_left_comment(node, &left_comment, &start) == 0) {
    if (is_multiline_block_comment(left_comment) ||
        is_empty_block_comment(left_comment) || is_line_comment(left_comment)) {
      return 0;
    }
  }
  return 1;
}

static int is_single_line_right_comments(int node) {
  int right_comment;
  int start = 0;
  while (get_right_comment(node, &right_comment, &start) == 0) {
    if (is_multiline_block_comment(right_comment) ||
        is_empty_block_comment(right_comment) ||
        is_line_comment(right_comment)) {
      return 0;
    }
  }
  return 1;
}

static void left_comments_after_then_write(int node, int indent) {
  const int is_single = is_single_line_left_comments(node);
  int left_comment;
  int start = 0;
  if (get_left_comment(node, &left_comment, &start)) {
    return;
  }
  comment_write(left_comment, indent);
  while (get_left_comment(node, &left_comment, &start) == 0) {
    if (is_single) {
      char_write(' ');
    } else {
      indent_write(indent);
    }
    comment_write(left_comment, indent);
  }
  indent_write(indent);
}

static void left_comments_with_spaces_write(
    // For example, if you have two single line block comments before the
    // body of a function they go on separate lines. But if they are before
    // a function argument they go on the same line.
    int is_multi_context, int node, int indent) {
  const int is_single = is_single_line_left_comments(node);
  int left_comment;
  int start = 0;
  if (get_left_comment(node, &left_comment, &start)) {
    return;
  }
  comment_write(left_comment, indent);
  while (get_left_comment(node, &left_comment, &start) == 0) {
    if (is_single && !is_multi_context) {
      char_write(' ');
    } else {
      indent_write(indent);
    }
    comment_write(left_comment, indent);
  }
  const int left_is_multiline = has_multiline_left_comment(node);
  if (left_is_multiline || is_multi_context) {
    indent_write(indent);
    return;
  }
  char_write(' ');
}

static int get_right_comment(int node, int *right_comment, int *i) {
  for (; *i < NUM_RIGHT_COMMENT; ++*i) {
    if (RIGHT_COMMENT_PARENT[*i] == (uint32_t)node) {
      *right_comment = RIGHT_COMMENT[*i];
      ++*i;
      return 0;
    }
  }
  return -1;
}

static void right_comments_in_expression_write(int node, int indent) {
  const int is_single = is_single_line_right_comments(node);
  int left_comment;
  int start = 0;
  if (get_right_comment(node, &left_comment, &start) == 0) {
    comment_write(left_comment, indent);
  }
  while (get_right_comment(node, &left_comment, &start) == 0) {
    if (is_single) {
      char_write(' ');
    } else {
      indent_write(indent);
    }
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
      char_write(' ');
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

static void right_comments_with_title_write(int node, int indent) {
  same_line_comment_write(node, indent);
  if (has_title_comment(node)) {
    char_write('\n');
  }
  title_comments_write(node, indent);
}

static void module_write(int node) {
  chunk_write("module X exposing (x)\n\n\nx =\n    ");
  left_comments_with_spaces_write(1, node, 4);
  expression_write(node, 4);
  char_write('\n');
}

static int format_in_memory() {
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

static int read_src() {
  FILE *file = fopen(PATH, "r");
  if (file == NULL) {
    return -1;
  }
  NUM_SRC = fread(SRC, 1, MAX_SRC, file);
  fclose(file);
  if (NUM_SRC == MAX_SRC) {
    fprintf(stderr, "file too large: %s, maximum size is %d bytes\n", PATH,
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

static int file_has_changed() {
  if (NUM_SRC != NUM_OUT) {
    return 1;
  }
  for (int i = 0; i < NUM_SRC; ++i) {
    if (SRC[i] != OUT[i]) {
      return 1;
    }
  }
  return 0;
}

static void format_file() {
  if (read_src()) {
    return;
  }
  // It used to work by writing the formatted code straight to file, with
  // fputc and fputs in the writer functions. At time of writing there are
  // 452 tests, all of them very small files, mostly less than 100B.

  // So all the time is pretty much spent in IO. I tried running gprof on it
  // a while ago, but all the time figures were zero. I think this means it
  // was mostly IO bound.

  // It currently takes 23 ms to format the test suite.

  // The original elm-format takes 54 ms.

  // I realised that a formatter mostly doesn't change the files, because on
  // a normal run most of the files are already formatted.

  // So I tried it with only writing to the file if it had changed. This
  // takes a bit more memory because I need to keep the formatted code in
  // memory.

  // But it now only takes 6ms.
  //                       ~

  calculate_triple_string_mask();
  calculate_row_numbers();
  calculate_column_numbers();

  const int result = format_in_memory();
  if (result) {
    fprintf(stderr, "could not format %s\n", PATH);
    return;
  }
  if (!file_has_changed()) {
    return;
  }
  FILE *out_file = fopen(PATH, "w");
  if (out_file == NULL) {
    return;
  }
  fwrite(OUT, 1, NUM_OUT, out_file);
  fclose(out_file);
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
    PATH = path;
    format_file();
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
