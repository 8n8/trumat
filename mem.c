#include <stdint.h>
#include <stdlib.h>
#include <stdio.h>

const int n = 150000000;

int main(int argc, char* argv[]) {
  uint8_t* mem = malloc(n);
  for (int i = 0; i < n; ++i) {
    mem[i] = i % 256;
  }
  int total = 0;
  for (int i = 0; i < n; ++i) {
    total += mem[i];
    total = total % 1000;
  }
  free(mem);
  printf("total: %d\n", total);
}
