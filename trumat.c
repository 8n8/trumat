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
    0 /*NUL*/, 0 /*SOH*/,   0 /*STX*/, 0 /*ETX*/,
    0 /*EOT*/, 0 /*ENQ*/,   0 /*ACK*/, 0 /*BEL*/,
    0 /*BS*/,  0 /*TAB*/,   0 /*LF*/,  0 /*VT*/,
    0 /*FF*/,  0 /*CR*/,    0 /*SO*/,  0 /*SI*/,
    0 /*DLE*/, 0 /*DC1*/,   0 /*DC2*/, 0 /*DC3*/,
    0 /*DC4*/, 0 /*NAK*/,   0 /*SYN*/, 0 /*ETB*/,
    0 /*CAN*/, 0 /*EM*/,    0 /*SUB*/, 0 /*ESC*/,
    0 /*FS*/,  0 /*GS*/,    0 /*RS*/,  0 /*RS*/,
    0 /*US*/,  0 /*Space*/, 0 /*!*/,   0 /*"*/,
    0 /*#*/,   0 /*$*/,     0 /*&*/,   0 /*'*/,
    0 /*(*/,   0 /*)*/,     0 /***/,   0 /*+*/,
    0 /*,*/,   0 /*-*/,     0 /*.*/,   0 /*forward slash*/,
    1 /*0*/,   1 /*1*/,     1 /*2*/,   1 /*3*/,
    1 /*4*/,   1 /*5*/,     1 /*6*/,   1 /*7*/,
    1 /*8*/,   1 /*9*/,     0 /*:*/,   0 /*;*/,
    0 /*<*/,   0 /*=*/,     0 /*>*/,   0 /*?*/,
    0 /*@*/,   0 /*A*/,     0 /*B*/,   0 /*C*/,
    0 /*D*/,   0 /*E*/,     0 /*F*/,   0 /*G*/,
    0 /*H*/,   0 /*I*/,     0 /*J*/,   0 /*K*/,
    0 /*L*/,   0 /*M*/,     0 /*N*/,   0 /*O*/,
    0 /*P*/,   0 /*Q*/,     0 /*R*/,   0 /*S*/,
    0 /*T*/,   0 /*U*/,     0 /*V*/,   0 /*W*/,
    0 /*X*/,   0 /*Y*/,     0 /*Z*/,   0 /*[*/,
    0 /*\*/,   0 /*]*/,     0 /*^*/,   0 /*_*/,
    0 /*`*/,   0 /*a*/,     0 /*b*/,   0 /*c*/,
    0 /*d*/,   0 /*e*/,     0 /*f*/,   0 /*g*/,
    0 /*h*/,   0 /*i*/,     0 /*j*/,   0 /*k*/,
    0 /*l*/,   0 /*m*/,     0 /*n*/,   0 /*o*/,
    0 /*p*/,   0 /*q*/,     0 /*r*/,   0 /*s*/,
    0 /*t*/,   0 /*u*/,     0 /*v*/,   0 /*w*/,
    0 /*x*/,   0 /*y*/,     0 /*z*/,   0 /*{*/,
    0 /*|*/,   0 /*}*/,     0 /*~*/,   0 /*DEL*/,
    0,         0,           0,         0,
    0,         0,           0,         0,
    0,         0,           0,         0,
    0,         0,           0,         0,
    0,         0,           0,         0,
    0,         0,           0,         0,
    0,         0,           0,         0,
    0,         0,           0,         0,
    0,         0,           0,         0,
    0,         0,           0,         0,
    0,         0,           0,         0,
    0,         0,           0,         0,
    0,         0,           0,         0,
    0,         0,           0,         0,
    0,         0,           0,         0,
    0,         0,           0,         0,
    0,         0,           0,         0,
    0,         0,           0,         0,
    0,         0,           0,         0,
    0,         0,           0,         0,
    0,         0,           0,         0,
    0,         0,           0,         0,
    0,         0,           0,         0,
    0,         0,           0,         0,
    0,         0,           0,         0,
    0,         0,           0,         0,
    0,         0,           0,         0,
    0,         0,           0,         0,
    0,         0,           0,         0,
    0,         0,           0,         0,
    0,         0,           0,         0,
    0,         0,           0,         0};

static const uint8_t is_normal_string_char[256] = {
    0 /*NUL*/,   0 /*SOH*/, 0 /*STX*/, 0 /*ETX*/,
    0 /*EOT*/,   0 /*ENQ*/, 0 /*ACK*/, 0 /*BEL*/,
    0 /*BS*/,    0 /*TAB*/, 0 /*LF*/,  0 /*VT*/,
    0 /*FF*/,    0 /*CR*/,  0 /*SO*/,  0 /*SI*/,
    0 /*DLE*/,   0 /*DC1*/, 0 /*DC2*/, 0 /*DC3*/,
    0 /*DC4*/,   0 /*NAK*/, 0 /*SYN*/, 0 /*ETB*/,
    0 /*CAN*/,   0 /*EM*/,  0 /*SUB*/, 0 /*ESC*/,
    0 /*FS*/,    0 /*GS*/,  0 /*RS*/,  0 /*US*/,
    1 /*Space*/, 1 /*!*/,   0 /*"*/,   1 /*#*/,
    1 /*$*/,     1 /*%*/,   1 /*&*/,   1 /*'*/,
    1 /*(*/,     1 /*)*/,   1 /***/,   1 /*+*/,
    1 /*,*/,     1 /*-*/,   1 /*.*/,   1 /*forward slash*/,
    1 /*0*/,     1 /*1*/,   1 /*2*/,   1 /*3*/,
    1 /*4*/,     1 /*5*/,   1 /*6*/,   1 /*7*/,
    1 /*8*/,     1 /*9*/,   1 /*:*/,   1 /*;*/,
    1 /*<*/,     1 /*=*/,   1 /*>*/,   1 /*?*/,
    1 /*@*/,     1 /*A*/,   1 /*B*/,   1 /*C*/,
    1 /*D*/,     1 /*E*/,   1 /*F*/,   1 /*G*/,
    1 /*H*/,     1 /*I*/,   1 /*J*/,   1 /*K*/,
    1 /*L*/,     1 /*M*/,   1 /*N*/,   1 /*O*/,
    1 /*P*/,     1 /*Q*/,   1 /*R*/,   1 /*S*/,
    1 /*T*/,     1 /*U*/,   1 /*V*/,   1 /*W*/,
    1 /*X*/,     1 /*Y*/,   1 /*Z*/,   1 /*[*/,
    0 /*\*/,     1 /*]*/,   1 /*^*/,   1 /*_*/,
    1 /*`*/,     1 /*a*/,   1 /*b*/,   1 /*c*/,
    1 /*d*/,     1 /*e*/,   1 /*f*/,   1 /*g*/,
    1 /*h*/,     1 /*i*/,   1 /*j*/,   1 /*k*/,
    1 /*l*/,     1 /*m*/,   1 /*n*/,   1 /*o*/,
    1 /*p*/,     1 /*q*/,   1 /*r*/,   1 /*s*/,
    1 /*t*/,     1 /*u*/,   1 /*v*/,   1 /*w*/,
    1 /*x*/,     1 /*y*/,   1 /*z*/,   1 /*{*/,
    1 /*|*/,     1 /*}*/,   1 /*~*/,   0 /*DEL*/,
    1,           1,         1,         1,
    1,           1,         1,         1,
    1,           1,         1,         1,
    1,           1,         1,         1,
    1,           1,         1,         1,
    1,           1,         1,         1,
    1,           1,         1,         1,
    1,           1,         1,         1,
    1,           1,         1,         1,
    1,           1,         1,         1,
    1,           1,         1,         1,
    1,           1,         1,         1,
    1,           1,         1,         1,
    1,           1,         1,         1,
    1,           1,         1,         1,
    1,           1,         1,         1,
    1,           1,         1,         1,
    1,           1,         1,         1,
    1,           1,         1,         1,
    1,           1,         1,         1,
    1,           1,         1,         1,
    1,           1,         1,         1,
    1,           1,         1,         1,
    1,           1,         1,         1,
    1,           1,         1,         1,
    1,           1,         1,         1,
    1,           1,         1,         1,
    1,           1,         1,         1,
    1,           1,         1,         1,
    1,           1,         1,         1,
    1,           1,         1,         1,
    1,           1,         1,         1};

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

static int parse_simple_string_piece(struct parser *p,
                                     struct text *expression) {
  int result = take_while_1(p, expression, is_normal_string_char);
  if (result == 0) {
    return result;
  }

  result = parse_chunk(p, "\\\"");
  if (result == 0) {
    return text_from_ascii("\\\"", expression, p->m);
  }

  result = parse_chunk(p, "\\\\");
  if (result == 0) {
    return text_from_ascii("\\\\", expression, p->m);
  }

  result = parse_chunk(p, "\\n");
  if (result == 0) {
    return text_from_ascii("\\n", expression, p->m);
  }

  result = parse_chunk(p, "\\t");
  if (result == 0) {
    return text_from_ascii("\\t", expression, p->m);
  }

  result = parse_chunk(p, "\\r");
  if (result == 0) {
    return text_from_ascii("\\u{000D}", expression, p->m);
  }

  return -1;
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
  while (parse_simple_string_piece(p, &contents) == 0) {
    result = text_join(*expression, contents, p->m, expression);
    if (result) {
      p->i = start;
      return result;
    }
  }

  result = parse_char(p, '"');
  if (result) {
    p->i = start;
    return result;
  }
  return text_append_ascii_char(*expression, '"', p->m, expression);
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
