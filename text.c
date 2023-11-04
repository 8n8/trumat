#include "text.h"

void text_zero_memory(struct text_memory *m) { m->head = 0; }

static int append_char(char ch, struct text_memory *m) {
  if (m->head == TEXT_SIZE) {
    return -1;
  }

  m->bytes[m->head] = ch;
  ++m->head;
  return 0;
}

int text_append_ascii_char(struct text left, char right, struct text *result,
                           struct text_memory *m) {

  if (left.end == m->head) {
    result->start = left.start;
    if (append_char(right, m)) {
      return -1;
    }
    result->end = m->head;
    return 0;
  }

  result->start = m->head;
  for (int i = left.start; i < left.end; ++i) {
    if (append_char(m->bytes[left.start + i], m)) {
      return -1;
    }
  }

  if (append_char(right, m)) {
    return -1;
  }
  result->end = m->head;

  return 0;
}
