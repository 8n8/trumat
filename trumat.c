#include "trumat.h"
#include <stdio.h>

void text_dbg(struct text, struct text_memory *);
int text_from_ascii(char *, struct text *, struct text_memory *);
int text_slice(struct text, int, int, struct text *);
int text_join(struct text, struct text, struct text *, struct text_memory *);
int text_append_ascii(struct text, const char *, struct text *,
                      struct text_memory *);
int text_append_ascii_char(struct text, char, struct text *,
                           struct text_memory *);

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

static int parse_char(struct text in, int *in_i, struct text_memory *m,
                      char ch) {
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
  result = text_append_ascii(*expression, ".0e", expression, m);
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

int format(const struct text in, struct text *out, struct text_memory *m) {
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

int string_equal(const char *a, const char *b) {
  for (; *a == *b && *a != 0 && *b != 0; ++a, ++b) {
  }

  return *a == 0 && *b == 0;
}

void text_zero_memory(struct text_memory *m) { m->head = 0; }

int text_length(struct text t) { return t.end - t.start; }

int text_from_file(FILE *file, struct text *t, struct text_memory *m) {
  t->start = m->head;
  size_t size = fread(m->bytes + t->start, 1, TEXT_SIZE - t->start, file);
  m->head += size;
  t->end = m->head;
  return 0;
}

int text_slice(struct text parent, int start, int end, struct text *result) {
  int parent_size = text_length(parent);
  if (start >= parent_size || end > parent_size || start > end) {
    return -1;
  }

  result->start = parent.start + start;
  result->end = parent.start + end;
  return 0;
}

void text_to_file(FILE *f, struct text t, struct text_memory *m) {
  fwrite(m->bytes + t.start, 1, t.end - t.start, f);
}

int text_index(struct text t, int index, struct text_memory *m) {
  if (index < 0 || t.start + index >= t.end || t.start + index >= TEXT_SIZE) {
    return -1;
  }

  return m->bytes[t.start + index];
}

static int append_char(char ch, struct text_memory *m) {
  if (m->head == TEXT_SIZE) {
    return -1;
  }

  m->bytes[m->head] = ch;
  ++m->head;
  return 0;
}

static int copy_to_head(struct text t, struct text_memory *m) {
  for (int i = 0;; ++i) {
    int result = text_index(t, i, m);
    if (result < 0) {
      break;
    }

    int err = append_char(result, m);
    if (err) {
      return err;
    }
  }
  return 0;
}

int text_append_ascii(struct text left, const char *right, struct text *result,
                      struct text_memory *m) {
  if (left.end == m->head) {
    result->start = left.start;
  } else {
    result->start = m->head;
  }

  if (left.end != m->head) {
    if (copy_to_head(left, m)) {
      return -1;
    }
  }

  for (int i = 0; right[i] != '\0'; ++i) {
    if (append_char(right[i], m)) {
      return -1;
    }
  }

  result->end = m->head;
  return 0;
}

int text_join(struct text left, struct text right, struct text *result,
              struct text_memory *m) {
  if (left.end == m->head) {
    result->start = left.start;
  } else {
    result->start = m->head;
  }

  if (left.end != m->head) {
    if (copy_to_head(left, m)) {
      return -1;
    }
  }

  if (copy_to_head(right, m)) {
    return -1;
  }

  result->end = m->head;
  return 0;
}

int text_append_ascii_char(struct text left, char right, struct text *result,
                           struct text_memory *m) {

  if (left.end == m->head) {
    result->start = left.start;
  } else {
    result->start = m->head;
  }

  if (left.end != m->head) {
    if (copy_to_head(left, m)) {
      return -1;
    }
  }

  if (append_char(right, m)) {
    return -1;
  }
  result->end = m->head;
  return 0;
}

int text_from_ascii(char *ascii, struct text *result, struct text_memory *m) {
  result->start = m->head;
  for (int i = 0; ascii[i] != '\0'; ++i) {
    if (append_char(ascii[i], m)) {
      return -1;
    }
  }

  result->end = m->head;
  return 0;
}

void text_dbg(struct text t, struct text_memory *m) {
  printf("DBG: ");
  for (int i = t.start; i < t.end; ++i) {
    putchar(m->bytes[i]);
  }
  printf("\n");
}
