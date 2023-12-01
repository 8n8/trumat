#include <dirent.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
// Should be enough to fit even a massive codebase
#define SRC_SIZE 100 * 1000 * 1000

// This contains all the source code of all the Elm files. It contains a
// sequence of <path>\0<Elm module>\0<path>\0<Elm module>\0\1
//
// So the paths and Elm modules are separated by null. The final module is
// terminated by \0\1. The characters \0 and \1 are not allowed in Elm code
// or paths.
uint8_t SRC[SRC_SIZE];

const char *usage = "expecting two arguments:\n"
                    "1. --overwrite to confirm it's OK to recursively "
                    "overwrite all the Elm files in the path\n"
                    "2. the path to format\n"
                    "\n"
                    "for example:\n"
                    "  $ trumat --overwrite Hello.elm\n"
                    "  $ trumat --overwrite some/directory/name\n";

static int string_length(char *s) {
  int n = 0;
  for (; *s != '\0'; ++s) {
    ++n;
  }
  return n;
}

int string_equal(char *a, char *b) {
  for (; *a == *b && *a != '\0'; ++a, ++b) {
  }
  return *a == '\0' && *b == '\0';
}

static void read_one_src(char *path, int *src_index) {
  FILE *file = fopen(path, "r");
  if (file == NULL) {
    return;
  }

  for (int i = 0; path[i] != '\0'; ++i) {
    SRC[*src_index] = path[i];
    ++*src_index;
  }
  SRC[*src_index] = '\0';

  int n = fread(SRC + *src_index, 1, SRC_SIZE - *src_index, file);
  if (!feof(file)) {
    fprintf(stderr, "Error: file %s is too big\n", path);
    exit(-1);
  }
  fclose(file);

  *src_index += n;
  SRC[*src_index] = '\0';
  ++*src_index;
}

static int is_elm_path(char *path) {
  int n = string_length(path);
  return n > 4 && path[n - 4] == '.' && path[n - 3] == 'e' &&
         path[n - 2] == 'l' && path[n - 1] == 'm';
}

static void make_sub_path(char *left, char *right, char *result) {
  int i = 0;
  for (; left[i] != '\0'; ++i) {
    result[i] = left[i];
  }
  result[i] = '/';
  ++i;
  int j = 0;
  for (; right[j] != '\0'; ++j) {
    result[i + j] = right[j];
  }
  i += j;
  result[i] = '\0';
}

static void read_src(char *path, int *src_index) {
  DIR *directory_handle = opendir(path);
  if (directory_handle == NULL && !is_elm_path(path)) {
    return;
  }

  if (directory_handle == NULL && is_elm_path(path)) {
    read_one_src(path, src_index);
    return;
  }

  struct dirent *entry = readdir(directory_handle);
  while (entry != NULL) {
    if (entry->d_name[0] != '.') {
      char sub_path[256];
      make_sub_path(path, entry->d_name, sub_path);
      read_src(sub_path, src_index);
    }
    entry = readdir(directory_handle);
  }
  closedir(directory_handle);

  ++*src_index;
  SRC[*src_index] = '\t';
}

int main(int argc, char *argv[]) {
  if (argc != 3 || !string_equal(argv[1], "--overwrite")) {
    fputs(usage, stderr);
    return -1;
  }

  int src_index = 0;
  read_src(argv[2], &src_index);

  return 0;
}
