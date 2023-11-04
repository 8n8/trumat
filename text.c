#include "text.h"

void text_zero_memory(struct text_memory *m) { m->head = 0; }

int text_append_ascii_char(struct text left, char right, struct text *result,
                           struct text_memory *m) {

  if (m->head == TEXT_SIZE) {
    return -1;
  }

  if (left.end == m->head) {
    result->start = left.start;
    m->bytes[m->head] = right;
    ++m->head;
    result->end = m->head;
    return 0;
  }

  result->start = m->head;
  for (int i = left.start; i < left.end; ++i) {
    m->bytes[m->head] = m->bytes[left.start + i];
    ++m->head;
  }

  m->bytes[m->head] = right;
  ++m->head;
  result->end = m->head;

  return 0;
}
