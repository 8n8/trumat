#include <stdio.h>
#include <stdint.h>
#include <string.h>
#include <stdlib.h>
#include "hydrogen.h"

#define MAX_SRC 1024
#define NUM_TESTS 10

struct params {
  uint32_t int_expression;
};

static void char_write(uint8_t src[MAX_SRC], int *size, char ch) {
  if (*size >= MAX_SRC) {
    fprintf(stderr, "src buffer overflow\n");
    exit(-1);
  }
  src[*size] = ch;
  ++*size;
}

static void chunk_write(uint8_t src[MAX_SRC], int *size, const char *chunk) {
  for (int i = 0; chunk[i] != '\0'; i++) {
    char_write(src, size, chunk[i]);
  }
}

static void int_write(uint8_t src[MAX_SRC], int *size, uint32_t n) {
  char buf[32];
  int len = sprintf(buf, "%u", n);
  for (int i = 0; i < len; i++) {
    char_write(src, size, buf[i]);
  }
}

static void generate_elm(struct params *p, uint8_t src[MAX_SRC], int *size) {
  chunk_write(src, size, "module X exposing (x)\n\n\nx =\n    ");
  int_write(src, size, p->int_expression);
  char_write(src, size, '\n');
}

static void generate_params(struct params *p) {
  p->int_expression = hydro_random_uniform(10000);
}

static void make_one_test(int i, uint8_t src[MAX_SRC]) {
  struct params p;
  generate_params(&p);
  int size = 0;
  generate_elm(&p, src, &size);
  char path[1024];
  sprintf(path, "tests/Test%05d.elm", i);
  FILE *f = fopen(path, "w");
  if (f == NULL) {
    fprintf(stderr, "Error: cannot open file %s\n", path);
  }
  fwrite(src, 1, size, f);
  fclose(f);
}

int main() {
  static uint8_t src[MAX_SRC];
  for (int i = 0; i < NUM_TESTS; i++) {
    make_one_test(i, src);
  }
}
