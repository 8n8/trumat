#include "trumat.h"
#include <dirent.h>
#include <stdio.h>
#include <sys/types.h>

void run_tests(char *, uint8_t in[1000000], uint8_t out[1000000]);
void run_one_test(char *, uint8_t in[1000000], uint8_t out[1000000]);
int is_elm_path(char *);
void make_sub_path(char *, char *, char *);
int is_dot_path(char *);
int string_length(char *);
void make_expected_path(char *, char *);

void make_expected_path(char *in_path, char *expected_path) {
  char *expected_root = "test_expected/";
  int expected_i = 0;
  for (; expected_root[expected_i] != 0; ++expected_i) {
    expected_path[expected_i] = expected_root[expected_i];
  }

  char *in_root = "test_input/";
  int in_i = 0;
  for (; in_root[in_i] != 0; ++in_i) {
  }

  for (; in_path[in_i] != 0;) {
    expected_path[expected_i] = in_path[in_i];
    ++expected_i;
    ++in_i;
  }

  expected_path[expected_i] = 0;
}

int is_dot_path(char *path) {
  int length = string_length(path);
  return (length == 1 && path[0] == '.') ||
         (length == 2 && path[0] == '.' && path[1] == '.');
}

void make_sub_path(char *parent, char *child, char *result) {
  int i = 0;
  for (; parent[i] != 0; ++i) {
    result[i] = parent[i];
  }
  result[i] = '/';
  ++i;

  int j = 0;
  for (; child[j] != 0; ++j) {
    result[i + j] = child[j];
  }
  i += j;
  result[i] = 0;
}

uint8_t IN[1000000];
uint8_t OUT[1000000];

int main(int argc, char *argv[]) { run_tests("test_input", IN, OUT); }

void run_tests(char *path, uint8_t in[1000000], uint8_t out[1000000]) {
  DIR *directory = opendir(path);
  struct dirent *item_in_directory;
  if (directory != NULL) {
    item_in_directory = readdir(directory);
    while (item_in_directory != NULL) {
      if (!is_dot_path(item_in_directory->d_name)) {
        char sub_path[256];
        make_sub_path(path, item_in_directory->d_name, sub_path);
        run_tests(sub_path, in, out);
      }
      item_in_directory = readdir(directory);
    }
    closedir(directory);
    return;
  }

  if (!is_elm_path(path)) {
    return;
  }

  run_one_test(path, in, out);
}

int string_length(char *path) {
  int i = 0;
  for (; path[i] != 0; ++i) {
  }
  return i;
}

int is_elm_path(char *path) {
  int length = string_length(path);
  return path[length - 1] == 'm' && path[length - 2] == 'l' &&
         path[length - 3] == 'e' && path[length - 4] == '.';
}

void run_one_test(char *in_path, uint8_t in[1000000], uint8_t out[1000000]) {
  char expected_path[256];
  make_expected_path(in_path, expected_path);
  puts(expected_path);
}
