#include <stdio.h>
#include <stdint.h>
#include <string.h>

#define MAX_SRC 1024
#define NUM_TESTS 1

struct params {
};

static void generate_elm(struct params *p, uint8_t src[MAX_SRC], int *size) {
  const char *elm =
    "module X exposing (x)\n"
    "\n"
    "\n"
    "x =\n"
    "    0\n";
  *size = strlen(elm);
  memcpy(src, elm, *size);
}

static void make_one_test(int i, uint8_t src[MAX_SRC]) {
  struct params p;
  int size;
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

int main(int argc, char *argv[]) {
  static uint8_t src[MAX_SRC];
  for (int i = 0; i < NUM_TESTS; i++) {
    make_one_test(i, src);
  }
}
