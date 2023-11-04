#include <stdint.h>
#include <stdio.h>
#define TEXT_SIZE 50 * 1000 * 1000

struct text {
  uint32_t start;
  uint32_t end;
};

struct text_memory {
  uint8_t bytes[TEXT_SIZE];
  uint32_t head;
};

int text_from_file(FILE *, struct text *, struct text_memory *);
void text_zero_memory(struct text_memory *);
int text_append_ascii_char(struct text, char, struct text *,
                           struct text_memory *);
int text_append_ascii(struct text, const char *, struct text *,
                      struct text_memory *);
int text_join(struct text, struct text, struct text *, struct text_memory *);
int text_index(struct text, int, struct text_memory);
int text_slice(struct text, int, int, struct text *);
int text_length(struct text);
