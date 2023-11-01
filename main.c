#include "trumat.h"
#include <dirent.h>
#include <stdio.h>
#include <sys/types.h>

uint8_t IN[CODE_SIZE];
uint8_t OUT[CODE_SIZE];
struct memory MEMORY;

static int code_length(const uint8_t code[CODE_SIZE]) {
  int i = 0;
  for (; code[i] != 0; ++i) {
  }
  return i;
}

void format_file(char *path, uint8_t in[CODE_SIZE], uint8_t out[CODE_SIZE],
                 struct memory *memory) {
  {
    FILE *file = fopen(path, "rb");
    if (file == NULL) {
      return;
    }
    int size = fread(in, 1, CODE_SIZE, file);
    in[size] = 0;
    fclose(file);

    int result = format(in, out, memory);

    if (result != 0) {
      fprintf(stderr,
              "could not format %s\nIt may be invalid Elm or it may be that "
              "this formatter doesn't support it yet.\n",
              path);
      return;
    }
  }

  FILE *file = fopen(path, "wb");
  fwrite(out, 1, code_length(out), file);

  printf("Processing %s\n", path);
}

void format_directory(char *path, uint8_t in[CODE_SIZE], uint8_t out[CODE_SIZE],
                      struct memory *memory) {
  DIR *directory = opendir(path);
  struct dirent *item_in_directory;
  if (directory != NULL) {
    item_in_directory = readdir(directory);
    while (item_in_directory != NULL) {
      if (!is_dot_path(item_in_directory->d_name)) {
        char sub_path[256];
        make_sub_path(path, item_in_directory->d_name, sub_path);
        format_directory(sub_path, in, out, memory);
      }
      item_in_directory = readdir(directory);
    }
  }

  if (!is_elm_path(path)) {
    return;
  }

  zero_memory(memory);
  format_file(path, in, out, memory);
}

char *usage =
    "you need to provide the --overwrite flag to confirm you are happy to "
    "recursively overwrite all the Elm files in this directory\n";

int string_equal(char *a, char *b) {
  for (; *a == *b && *a != 0 && *b != 0; ++a, ++b) {
  }

  return *a == 0 && *b == 0;
}

int is_valid_args(int argc, char *argv[]) {
  if (argc != 2) {
    return 0;
  }

  return string_equal("--overwrite", argv[1]);
}

int main(int argc, char *argv[]) {
  if (!is_valid_args(argc, argv)) {
    fputs(usage, stderr);
    return -1;
  }

  format_directory(".", IN, OUT, &MEMORY);
}
