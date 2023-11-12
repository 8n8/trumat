#include "trumat.h"
#include <stdio.h>

struct parser {
  struct text in;
  int i;
  struct text_memory *m;
};

static int parse_int(struct parser *, struct text *);

void text_dbg(struct text t, struct text_memory *m) {
  printf("DBG: ");
  for (int i = t.start; i < t.end; ++i) {
    putchar(m->bytes[i]);
  }
  printf("\n");
}

static int append_char(char ch, struct text_memory *m) {
  if (m->head == TEXT_SIZE) {
    return -1;
  }

  m->bytes[m->head] = ch;
  ++m->head;
  return 0;
}

static int text_slice(struct text parent, int start, int end,
                      struct text *result) {
  int parent_size = text_length(parent);
  if (start >= parent_size || end > parent_size || start > end) {
    return -1;
  }

  result->start = parent.start + start;
  result->end = parent.start + end;
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

static int text_join(struct text left, struct text right, struct text_memory *m,
                     struct text *result) {
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

static int text_append_ascii(struct text left, const char *right,
                             struct text *result, struct text_memory *m) {
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

static int text_prepend_ascii_char(char left, struct text right,
                                   struct text_memory *m, struct text *result) {
  result->start = m->head;
  if (append_char(left, m)) {
    return -1;
  }

  if (copy_to_head(right, m)) {
    return -1;
  }
  result->end = m->head;
  return 0;
}

static int text_append_ascii_char(struct text left, char right,
                                  struct text_memory *m, struct text *result) {

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

static int text_from_ascii(char *ascii, struct text *result,
                           struct text_memory *m) {
  result->start = m->head;
  for (int i = 0; ascii[i] != '\0'; ++i) {
    if (append_char(ascii[i], m)) {
      return -1;
    }
  }

  result->end = m->head;
  return 0;
}

static int text_from_ascii_char(char ascii, struct text *result,
                           struct text_memory *m) {
  result->start = m->head;
  if (append_char(ascii, m)) {
    return -1;
  }

  result->end = m->head;
  return 0;
}

static int parse_chunk(struct parser *p, const char *chunk) {

  int start = p->i;

  int i = 0;
  for (; text_index(p->in, p->i, p->m) == chunk[i] && chunk[i] != 0;
       ++p->i, ++i) {
  }

  if (chunk[i] == 0) {
    return 0;
  }

  p->i = start;
  return -1;
}

static int take_while(struct parser *p, struct text *matching,
                        const uint8_t match[256]) {

  int start = p->i;

  for (; match[text_index(p->in, p->i, p->m)]; ++p->i) {
  }

  return text_slice(p->in, start, p->i, matching);
}

static int take_while_1(struct parser *p, struct text *matching,
                        const uint8_t match[256]) {

  int start = p->i;

  if (!(match[text_index(p->in, p->i, p->m)])) {
    return -1;
  }
  ++p->i;

  for (; match[text_index(p->in, p->i, p->m)]; ++p->i) {
  }

  return text_slice(p->in, start, p->i, matching);
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

static const uint8_t is_alpha_num[256] = {0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0 };

static int parse_char(struct parser *p, char ch) {
  int got = text_index(p->in, p->i, p->m);
  if (got < 0) {
    return -1;
  }

  if (got != ch) {
    return -1;
  }

  ++p->i;
  return 0;
}

static int parse_positive_int(struct parser *p, struct text *expression) {
  return take_while_1(p, expression, is_digit);
}

static int parse_negative_int(struct parser *p, struct text *expression) {
  int start = p->i;

  int result = parse_char(p, '-');
  if (result) {
    p->i = start;
    return result;
  }

  struct text exponent;
  result = parse_positive_int(p, &exponent);
  if (result) {
    p->i = start;
    return result;
  }

  return text_prepend_ascii_char('-', exponent, p->m, expression);
}

static int parse_int(struct parser *p, struct text *expression) {

  int result = parse_positive_int(p, expression);
  if (result == 0) {
    return 0;
  }

  return parse_negative_int(p, expression);
}

static int parse_simple_float(struct parser *p, struct text *expression) {

  int start = p->i;

  int result = take_while_1(p, expression, is_digit);
  if (result) {
    p->i = start;
    return result;
  }

  result = parse_char(p, '.');
  if (result) {
    p->i = start;
    return result;
  }
  result = text_append_ascii_char(*expression, '.', p->m, expression);
  if (result) {
    p->i = start;
    return result;
  }

  struct text after_dot;
  result = take_while_1(p, &after_dot, is_digit);
  if (result) {
    p->i = start;
    return result;
  }

  return text_join(*expression, after_dot, p->m, expression);
}

static int parse_float_exponent(struct parser *p, struct text *expression) {
  int start = p->i;

  int result = parse_char(p, 'e');
  if (result) {
    p->i = start;
    return result;
  }

  struct text exponent;
  result = parse_int(p, &exponent);
  if (result) {
    p->i = start;
    return result;
  }

  return text_prepend_ascii_char('e', exponent, p->m, expression);
}

static int parse_dot_exponent_float(struct parser *p, struct text *expression) {
  int start = p->i;

  struct text before_exponent;
  int result = parse_simple_float(p, &before_exponent);
  if (result) {
    p->i = start;
    return result;
  }

  result = text_join(*expression, before_exponent, p->m, expression);
  if (result) {
    p->i = start;
    return result;
  }

  struct text exponent;
  result = parse_float_exponent(p, &exponent);
  if (result) {
    p->i = start;
    return result;
  }
  return text_join(*expression, exponent, p->m, expression);
}

static int parse_non_dot_exponent_float(struct parser *p,
                                        struct text *expression) {

  int start = p->i;

  int result = take_while_1(p, expression, is_digit);
  if (result) {
    p->i = start;
    return result;
  }

  struct text exponent;
  result = parse_float_exponent(p, &exponent);
  if (result) {
    p->i = start;
    return result;
  }

  result = text_append_ascii(*expression, ".0", expression, p->m);
  if (result) {
    p->i = start;
    return result;
  }

  return text_join(*expression, exponent, p->m, expression);
}

static int parse_positive_float(struct parser *p, struct text *expression) {
  int start = p->i;

  int result = parse_dot_exponent_float(p, expression);
  if (result == 0) {
    return 0;
  }

  result = parse_simple_float(p, expression);
  if (result == 0) {
    return 0;
  }

  result = parse_non_dot_exponent_float(p, expression);
  if (result == 0) {
    return 0;
  }

  p->i = start;
  return -1;
}

static int parse_negative_float(struct parser *p, struct text *expression) {
  int start = p->i;

  int result = parse_char(p, '-');
  if (result) {
    p->i = start;
    return result;
  }

  struct text positive;
  result = parse_positive_float(p, &positive);
  if (result) {
    p->i = start;
    return result;
  }

  return text_prepend_ascii_char('-', positive, p->m, expression);
}

static int parse_float(struct parser *p, struct text *expression) {
  int result = parse_positive_float(p, expression);
  if (result == 0) {
    return 0;
  }

  return parse_negative_float(p, expression);
}

static int parse_simple_string(struct parser *p, struct text *expression) {
  int start = p->i;

  int result = parse_char(p, '"');
  if (result) {
    p->i = start;
    return result;
  }

  result = text_from_ascii_char('"', expression, p->m);
  if (result) {
    p->i = start;
    return result;
  }

  struct text contents;
  result = take_while(p, &contents, is_alpha_num);
  if (result) {
    p->i = start;
    return result;
  }
  result = text_join(*expression, contents, p->m, expression);
  if (result) {
    p->i = start;
    return result;
  }

  result = parse_char(p, '"');
  if (result) {
    p->i = start;
    return result;
  }

  result = text_append_ascii_char(*expression, '"', p->m, expression);
  if (result) {
    p->i = start;
    return result;
  }

  return 0;
}

static int parse_expression(struct parser *p, struct text *expression) {

  int result = parse_float(p, expression);
  if (result == 0) {
    return 0;
  }

  result = parse_int(p, expression);
  if (result == 0) {
    return 0;
  }

  return parse_simple_string(p, expression);
}

int format(const struct text in, struct text *out, struct text_memory *m) {
  struct parser p = {.in = in, .i = 0, .m = m};
  int result = parse_chunk(&p, "module X exposing (x)\n\n\nx =\n");
  if (result < 0) {
    return result;
  }

  for (; text_index(p.in, p.i, p.m) == ' '; ++p.i) {
  }

  struct text expression;
  result = parse_expression(&p, &expression);
  if (result < 0) {
    return result;
  }

  result = parse_chunk(&p, "\n\0");
  if (result < 0) {
    return result;
  }

  result = text_from_ascii("module X exposing (x)\n\n\nx =\n    ", out, p.m);
  if (result) {
    return result;
  }
  result = text_join(*out, expression, p.m, out);
  if (result) {
    return result;
  }
  result = text_append_ascii_char(*out, '\n', p.m, out);
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

void text_to_file(FILE *f, struct text t, struct text_memory *m) {
  fwrite(m->bytes + t.start, 1, t.end - t.start, f);
}

int text_index(struct text t, int index, struct text_memory *m) {
  if (index < 0 || t.start + index >= t.end || t.start + index >= TEXT_SIZE) {
    return -1;
  }

  return m->bytes[t.start + index];
}
