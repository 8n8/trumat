#include "trumat.h"
#include <stdio.h>

// Checks that some text contains an ASCII string, starting at an index.
//
// It returns a negative number if it's not there.
//
// It returns the position of end of the searched-for string.
//
// To be clear, the searched for string must start at the provided position.
static int parse_chunk(
    // The text that is searched.
    struct text in,
    // The position to start the search.
    int in_i,
    // The string to search for.
    const char *chunk, struct text_memory *m) {

  int i = 0;
  for (; text_index(in, i + in_i, m) == chunk[i] && chunk[i] != 0; ++i) {
  }

  if (chunk[i] == 0) {
    return in_i + i;
  }

  return -1;
}

static int take_while_1(const struct text in, int *in_i, struct text_memory *m,
                        struct text *matching, const uint8_t match[256]) {

  int start = *in_i;

  if (!(match[text_index(in, *in_i, m)])) {
    return -1;
  }
  ++*in_i;

  for (; match[text_index(in, *in_i, m)]; ++*in_i) {
  }

  return text_slice(in, start, *in_i, matching);
}

static const uint8_t is_digit[256] = {
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0};

static int parse_char(struct text in, int *in_i, struct text_memory *m, char ch) {
  int got = text_index(in, *in_i, m);
  if (got < 0) {
    return -1;
  }

  if (got != ch) {
    return -1;
  }

  ++*in_i;
  return 0;
}

static int parse_simple_float(struct text in, int *in_i, struct text_memory *m,
                       struct text *expression) {

  int start = *in_i;

  int result = take_while_1(in, in_i, m, expression, is_digit);
  if (result) {
    *in_i = start;
    return result;
  }

  result = parse_char(in, in_i, m, '.');
  if (result) {
    *in_i = start;
    return result;
  }
  result = text_append_ascii_char(*expression, '.', expression, m);
  if (result) {
    *in_i = start;
    return result;
  }

  struct text after_dot;
  result = take_while_1(in, in_i, m, &after_dot, is_digit);
  if (result) {
    *in_i = start;
    return result;
  }

  return text_join(*expression, after_dot, expression, m);
}

static int parse_non_dot_exponent_float(struct text in, int *in_i,
                                 struct text_memory *m,
                                 struct text *expression) {

  int start = *in_i;

  int result = take_while_1(in, in_i, m, expression, is_digit);
  if (result) {
    *in_i = start;
    return result;
  }

  result = parse_char(in, in_i, m, 'e');
  if (result) {
    *in_i = start;
    return result;
  }
  result = text_append_ascii_char(*expression, 'e', expression, m);
  if (result) {
    *in_i = start;
    return result;
  }

  struct text after_e;
  result = take_while_1(in, in_i, m, &after_e, is_digit);
  if (result) {
    *in_i = start;
    return result;
  }

  return text_join(*expression, after_e, expression, m);
}

static int parse_float(struct text in, int *in_i, struct text_memory *m,
                       struct text *expression) {
  int start = *in_i;

  int result = parse_simple_float(in, in_i, m, expression);
  if (result == 0) {
    return 0;
  }

  result = parse_non_dot_exponent_float(in, in_i, m, expression);
  if (result == 0) {
    return 0;
  }

  *in_i = start;
  return -1;
}

static int parse_expression(struct text in, int *in_i, struct text_memory *m,
                            struct text *expression) {

  int result = parse_float(in, in_i, m, expression);
  if (result == 0) {
    return 0;
  }

  return take_while_1(in, in_i, m, expression, is_digit);
}

int format(struct text in, struct text *out, struct text_memory *m) {
  int in_i = 0;
  int result = parse_chunk(in, in_i, "module X exposing (x)\n\n\nx =\n", m);
  if (result < 0) {
    return result;
  }
  in_i = result;

  for (; text_index(in, in_i, m) == ' '; ++in_i) {
  }

  struct text expression;
  result = parse_expression(in, &in_i, m, &expression);
  if (result < 0) {
    return result;
  }

  result = parse_chunk(in, in_i, "\n\0", m);
  if (result < 0) {
    return result;
  }

  result = text_from_ascii("module X exposing (x)\n\n\nx =\n    ", out, m);
  if (result) {
    return result;
  }
  result = text_join(*out, expression, out, m);
  if (result) {
    return result;
  }
  result = text_append_ascii_char(*out, '\n', out, m);
  if (result) {
    return result;
  }

  return 0;
}

void make_sub_path(const char *parent, const char *child, char *result) {
  int i = 0;
  for (; parent[i] != 0; ++i) {
    result[i] = parent[i];
  }
  result[i] = '/';
  ++i;

  int j = 0;
  for (; child[j] != 0; ++j) {
    result[i + j] = child[j];
  }
  i += j;
  result[i] = 0;
}

static int string_length(const char *path) {
  int i = 0;
  for (; path[i] != 0; ++i) {
  }
  return i;
}

int is_dot_path(const char *path) {
  int length = string_length(path);
  return (length == 1 && path[0] == '.') ||
         (length == 2 && path[0] == '.' && path[1] == '.');
}

int is_elm_path(const char *path) {
  int length = string_length(path);
  return length >= 4 && path[length - 1] == 'm' && path[length - 2] == 'l' &&
         path[length - 3] == 'e' && path[length - 4] == '.';
}

int string_equal(char *a, char *b) {
  for (; *a == *b && *a != 0 && *b != 0; ++a, ++b) {
  }

  return *a == 0 && *b == 0;
}
