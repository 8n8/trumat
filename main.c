#include "trumat.h"
#include <dirent.h>
#include <stdio.h>
#include <sys/types.h>

struct text_memory MEMORY;

void format_file(char *path, struct text_memory *m) {
  struct text out;
  {
    FILE *file = fopen(path, "rb");
    if (file == NULL) {
      return;
    }
    struct text in;
    if (text_from_file(file, &in, m)) {
      fprintf(stderr, "could not read the file: %s\n", path);
      return;
    }

    int result = format(in, &out, m);

    if (result != 0) {
      fprintf(stderr,
              "could not format %s\nIt may be invalid Elm or it may be that "
              "this formatter doesn't support it yet.\n",
              path);
      return;
    }
  }

  FILE *file = fopen(path, "wb");
  text_to_file(file, out, m);

  printf("Processing %s\n", path);
}

void format_directory(char *path, struct text_memory *memory) {
  DIR *directory = opendir(path);
  struct dirent *item_in_directory;
  if (directory != NULL) {
    item_in_directory = readdir(directory);
    while (item_in_directory != NULL) {
      if (!is_dot_path(item_in_directory->d_name)) {
        char sub_path[256];
        make_sub_path(path, item_in_directory->d_name, sub_path);
        format_directory(sub_path, memory);
      }
      item_in_directory = readdir(directory);
    }
  }
  closedir(directory);

  if (!is_elm_path(path)) {
    return;
  }

  text_zero_memory(memory);
  format_file(path, memory);
}

char *usage =
    "you need to provide the --overwrite flag to confirm you are happy to "
    "recursively overwrite all the Elm files in this directory\n";

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

  format_directory(".", &MEMORY);
}
