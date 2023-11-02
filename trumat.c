#include "trumat.h"
#include <stdio.h>

void zero_memory(struct memory *m) { m->text_head = 0; }

// Write a string to an array of bytes at a particular index. The return
// value is the first index after the end of the new section.
//
// If there isn't enough room in the array then it writes as much as it can.
static int write_chunk(
    // The string is written into this array, starting at the given
    // position.
    uint8_t array[CODE_SIZE],
    // This is the position to write the first byte of the chunk to.
    int position,
    // This is the string that is written to the array.
    const char *chunk) {

  int chunk_i = 0;
  for (; position + chunk_i < CODE_SIZE && chunk[chunk_i] != 0; ++chunk_i) {
    array[position + chunk_i] = chunk[chunk_i];
  }
  return position + chunk_i;
}

// Checks that an array contains a string, starting at an index.
//
// It returns a negative number if the string isn't in the array, starting
// at the provided position.
//
// It returns the position of the end of the string in the array if it is
// there.
//
// To be clear, the searched for string must start at the provided position.
// It can't come later in the array.
static int parse_chunk(
    // The array to search
    const uint8_t in[CODE_SIZE],
    // The position in the array to start the search.
    int in_i,
    // The chunk to search for.
    const char *chunk) {

  int i = 0;
  for (; in_i + i < CODE_SIZE && in[i + in_i] == chunk[i] && chunk[i] != 0;
       ++i) {
  }

  if (chunk[i] == 0) {
    return in_i + i;
  }

  return -1;
}

static int text_from_bytes(const uint8_t in[CODE_SIZE], int start, int end,
                           struct memory *m) {
  int string = m->text_head;
  int i = 0;
  for (; start + i < end && m->text_head + i < TEXT_SIZE; ++i) {
    m->text[m->text_head + i] = in[start + i];
  }

  if (i + start + 1 < end || m->text_head == TEXT_SIZE) {
    return -1;
  }

  m->text_head += i;

  m->text[m->text_head] = 0;
  ++m->text_head;

  return string;
}

static int take_while_1(const uint8_t in[CODE_SIZE], int *in_i,
                        struct memory *m, int *string,
                        const uint8_t match[256]) {

  int start = *in_i;

  if (!(*in_i < CODE_SIZE && match[in[*in_i]])) {
    return -1;
  }
  ++*in_i;

  for (; match[in[*in_i]] && *in_i < CODE_SIZE; ++*in_i) {
  }

  int result = text_from_bytes(in, start, *in_i, m);
  if (result < 0) {
    return result;
  }

  *string = result;

  return 0;
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

static int write_text(uint8_t out[CODE_SIZE], int out_i, int text,
                      struct memory *m) {

  int i = 0;
  for (; m->text[text + i] != 0; ++i) {
    out[out_i + i] = m->text[text + i];
  }

  return out_i + i;
}

int format(const uint8_t in[CODE_SIZE], uint8_t out[CODE_SIZE],
           struct memory *m) {

  int in_i = 0;
  int result = parse_chunk(in, in_i, "module X exposing (x)\n\n\nx =\n");
  if (result < 0) {
    return result;
  }
  in_i = result;

  for (; in[in_i] == ' '; ++in_i) {
  }

  int int_literal;
  result = take_while_1(in, &in_i, m, &int_literal, is_digit);
  if (result < 0) {
    return result;
  }

  result = parse_chunk(in, in_i, "\n\0");
  if (result < 0) {
    return result;
  }

  int out_i = write_chunk(out, 0, "module X exposing (x)\n\n\nx =\n    ");
  out_i = write_text(out, out_i, int_literal, m);
  out_i = write_chunk(out, out_i, "\n");
  out[out_i] = 0;

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
