#include "trumat.h"

int text_equal(struct text a, struct text b) {
  if (text_length(a) != text_length(b)) {
    return 0;
  }

  int i = 0;
  for (; text_index(a, i) >= 0 && text_index(a, i) == text_index(b, i); ++i) {
  }

  return text_length(a) == i;
}

int format(
  uint8_t in[NUM_SRC],
  int in_size,
  uint8_t out[NUM_SRC],
  int * out_size,
  struct ast * ast) {

  out = in;
  *out = in_size;

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
