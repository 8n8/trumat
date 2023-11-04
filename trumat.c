#include "trumat.h"
#include <stdio.h>

void zero_memory(struct memory *m) { text_zero_memory(&m->text); }

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
    const char *chunk,
    const struct text_memory m) {

  int i = 0;
  for (; text_index(in, i + in_i, m) == chunk[i] && chunk[i] != 0;
       ++i) {
  }

  if (chunk[i] == 0) {
    return in_i + i;
  }

  return -1;
}

static int take_while_1(const struct text in, int *in_i,
                        struct text_memory m, struct text *matching,
                        const uint8_t match[256]) {

  int start = *in_i;

  if (!(match[text_index(in, *in_i, m)])) {
    return -1;
  }
  ++*in_i;

  for (; match[text_index(in, *in_i, m)]; ++*in_i) {
  }

  return text_slice(in, start, *in_i, matching, m);
}

const uint8_t is_digit[256] = {
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

int format(struct text in, struct text *out, struct memory *m) {

  int in_i = 0;
  int result = parse_chunk(in, in_i, "module X exposing (x)\n\n\nx =\n", m->text);
  if (result < 0) {
    return result;
  }
  in_i = result;

  for (; text_index(in, in_i, m->text) == ' '; ++in_i) {
  }

  struct text int_literal;
  result = take_while_1(in, &in_i, m->text, &int_literal, is_digit);
  if (result < 0) {
    return result;
  }

  result = parse_chunk(in, in_i, "\n\0", m->text);
  if (result < 0) {
    return result;
  }

  result = text_append_ascii(*out, "module X exposing (x)\n\n\nx =\n    ", out, &m->text);
  if (result) {
    return result;
  }
  result = text_join(*out, int_literal, out, &m->text);
  if (result) {
    return result;
  }
  result = text_append_ascii_char(*out, '\n', out, &m->text);
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
