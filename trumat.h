#include <stdint.h>
#include <stdio.h>
#define TEXT_SIZE 50 * 1000 * 1000

struct text {
  uint32_t start;
  uint32_t end;
};

void init_memory();
int text_from_file(FILE *, struct text *);
void text_zero_memory();
int text_index(struct text, int);
int text_length(struct text);
void text_to_file(FILE *, struct text);
void text_to_string(struct text, char string[300]);
int text_equal(struct text, struct text);

int string_equal(const char *, const char *);

int format(const struct text in, struct text *out);

void make_sub_path(const char *parent, const char *child, char *result);

// Checks if the path is "." or "..".
int is_dot_path(const char *);

// Checks if the path ends in .elm
int is_elm_path(const char *);
