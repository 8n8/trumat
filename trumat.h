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
int text_index(struct text, int, struct text_memory *);
int text_length(struct text);
void text_to_file(FILE *, struct text, struct text_memory *);

int string_equal(const char *, const char *);

// This is the formatter.
int format(
    // This should contain the unformatted source code.
    const struct text in,
    // This will be overwritten with the formatted code.
    struct text *out, struct text_memory *m);

// Writes "parent" and "child" to "result", separated by a forward slash.
void make_sub_path(const char *parent, const char *child, char *result);

// Checks if the path is "." or "..".
int is_dot_path(const char *);

// Checks if the path ends in .elm
int is_elm_path(const char *);
