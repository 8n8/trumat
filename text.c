#include "text.h"

void text_zero_memory(struct text_memory *m) { m->head = 0; }

int text_append_ascii_char(struct text base, char ch, struct text *result,
                           struct text_memory *m) {

  if (m->head == TEXT_SIZE) {
    return -1;
  }

  if (base.end == m->head) {
    result->start = base.start;
    m->bytes[m->head] = ch;
    ++m->head;
    result->end = m->head;
    return 0;
  }

  result->start = m->head;
  for (int i = base.start; i < base.end; ++i) {
    m->bytes[m->head] = m->bytes[base.start + i];
    ++m->head;
  }

  m->bytes[m->head] = ch;
  ++m->head;
  result->end = m->head;

  return 0;
}
