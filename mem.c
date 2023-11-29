#include <stdint.h>
#include <stdlib.h>
#include <stdio.h>

int run_one(int counter) {
  uint8_t* mem = malloc(5000000);
  for (int i = 0; i < 5000000; ++i) {
    mem[i] = (counter + i) % 256;
  }

  int total = 0;
  for (int i = 0; i < 5000000; ++i) {
    total += mem[i];
  }
  free(mem);

  return total;
}

int main(int argc, char* argv[]) {
  int total = 0;
  for (int i = 0; i < 5000; ++i) {
    total += run_one(i);
  }

  printf("total: %d\n", total);
}
