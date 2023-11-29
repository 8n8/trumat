#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#define TEXT_SIZE 50 * 1000 * 1000
#define NUM_NODES 500 * 1000
#define NUM_TEXT 100 * 1000
#define NUM_SRC 1500 * 1000

struct ast {
  // The children are the indexes. The parents are in the array. The first
  // 24 bytes of each item is the node counter. The final 8 bits is the
  // type.
  uint32_t parents[NUM_NODES];

  // The node counters are the indexes. The items are the start indexes in
  // the source code. It aligns with src_size.
  uint32_t src_start[NUM_NODES];

  // The node counters are the indexes. The items are the sizes of the text
  // chunks in the source code. It aligns with src_start.
  uint16_t src_size[NUM_NODES];
};

struct text {
  uint32_t start;
  uint32_t end;
};

void dbg();
void dbg_verbose();
void init_memory();
int text_from_file(FILE *, struct text *);
void text_zero_memory();
int text_index(struct text, int);
int text_length(struct text);
void text_to_file(FILE *, struct text);
void text_to_string(struct text, char string[300]);
int text_equal(struct text, struct text);

int string_equal(const char *, const char *);

int format(
    const uint8_t[NUM_SRC],
    const int,
    uint8_t[NUM_SRC],
    int *,
    struct ast *);

// int format(const struct text in, struct text *out);

void make_sub_path(const char *parent, const char *child, char *result);

// Checks if the path is "." or "..".
int is_dot_path(const char *);

// Checks if the path ends in .elm
int is_elm_path(const char *);
