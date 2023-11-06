#include "text.h"

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
