#include "trumat.h"
#include <dirent.h>
#include <stdio.h>
#include <sys/types.h>

void run_positive_tests(char *, uint8_t in[1000000], uint8_t out[1000000],
                        struct memory *);
void run_no_change_tests(char *, uint8_t in[1000000], uint8_t out[1000000],
                         struct memory *);
void run_one_positive_test(char *, uint8_t in[1000000], uint8_t out[1000000],
                           struct memory *);
void run_one_no_change_test(char *, uint8_t in[1000000], uint8_t out[1000000],
                            struct memory *);
int is_elm_path(char *);
void make_sub_path(char *, char *, char *);
int is_dot_path(char *);
int string_length(char *);
void make_expected_path(char *, char *);
void print_error(char *, char *);
void check_expected(char *, uint8_t out[1000000]);

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
struct memory MEMORY;

int main(int argc, char *argv[]) {
  run_positive_tests("test_input", IN, OUT, &MEMORY);
  zero_memory(&MEMORY);
  run_no_change_tests("test_formatted", IN, OUT, &MEMORY);
}

void run_no_change_tests(char *path, uint8_t in[1000000], uint8_t out[1000000],
                         struct memory *memory) {
  DIR *directory = opendir(path);
  struct dirent *item_in_directory;
  if (directory != NULL) {
    item_in_directory = readdir(directory);
    while (item_in_directory != NULL) {
      if (!is_dot_path(item_in_directory->d_name)) {
        char sub_path[256];
        make_sub_path(path, item_in_directory->d_name, sub_path);
        run_no_change_tests(sub_path, in, out, memory);
      }
      item_in_directory = readdir(directory);
    }
    closedir(directory);
    return;
  }

  if (!is_elm_path(path)) {
    return;
  }

  zero_memory(memory);
  run_one_no_change_test(path, in, out, memory);
}

void run_positive_tests(char *path, uint8_t in[1000000], uint8_t out[1000000],
                        struct memory *memory) {
  DIR *directory = opendir(path);
  struct dirent *item_in_directory;
  if (directory != NULL) {
    item_in_directory = readdir(directory);
    while (item_in_directory != NULL) {
      if (!is_dot_path(item_in_directory->d_name)) {
        char sub_path[256];
        make_sub_path(path, item_in_directory->d_name, sub_path);
        run_positive_tests(sub_path, in, out, memory);
      }
      item_in_directory = readdir(directory);
    }
    closedir(directory);
    return;
  }

  if (!is_elm_path(path)) {
    return;
  }

  zero_memory(memory);
  run_one_positive_test(path, in, out, memory);
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

void run_one_no_change_test(char *in_path, uint8_t in[1000000],
                            uint8_t out[1000000], struct memory *memory) {
  FILE *in_file = fopen(in_path, "rb");
  if (in_file == NULL) {
    char error_message[256];
    sprintf(error_message, "could not open file: %s", in_path);
    print_error(in_path, error_message);
    return;
  }
  int in_size = fread(in, 1, 1000000, in_file);
  in[in_size] = 0;

  int result = format(in, out, memory);

  if (result != 0) {
    printf("SUCCESS: formatter error code but OK: %s\n", in_path);
    return;
  }

  for (int i = 0; i < 1000000; ++i) {
    if (in[i] == 0 && out[i] == 0) {
      break;
    }

    if (in[i] == 0) {
      print_error(in_path, "output is too long");
      return;
    }

    if (out[i] == 0) {
      print_error(in_path, "output is too short");
      return;
    }

    if (in[i] != out[i]) {
      char error_message[300];
      sprintf(error_message, "expected '%c' but got '%c' at position %d", in[i],
              out[i], i);
      print_error(in_path, error_message);
      return;
    }
  }

  printf("SUCCESS: %s\n", in_path);
}

void run_one_positive_test(char *in_path, uint8_t in[1000000],
                           uint8_t out[1000000], struct memory *memory) {
  FILE *in_file = fopen(in_path, "rb");
  if (in_file == NULL) {
    char error_message[256];
    sprintf(error_message, "could not open file: %s", in_path);
    print_error(in_path, error_message);
    return;
  }
  int in_size = fread(in, 1, 1000000, in_file);
  in[in_size] = 0;

  int result = format(in, out, memory);

  if (result != 0) {
    char error_message[256];
    sprintf(error_message, "formatter failed with non-zero result: %d", result);
    print_error(in_path, error_message);
    return;
  }

  check_expected(in_path, out);
}

void check_expected(char *in_path, uint8_t out[1000000]) {
  char expected_path[256];
  make_expected_path(in_path, expected_path);

  FILE *expected_file = fopen(expected_path, "rb");
  if (expected_file == NULL) {
    char error_message[300];
    sprintf(error_message, "could not open file; %s", expected_path);
    print_error(expected_path, error_message);
    return;
  }

  for (int i = 0; i < 1000000; ++i) {
    int expected = fgetc(expected_file);
    if (expected == EOF && out[i] == 0) {
      break;
    }

    if (expected != out[i]) {
      char error_message[300];
      sprintf(error_message, "expected '%c' but got '%c' at position %d",
              expected, out[i], i);
      print_error(in_path, error_message);
      return;
    }
  }

  printf("SUCCESS: %s\n", in_path);
}

void print_error(char *path, char *message) {
  printf("FAILED: %s\n\n    %s\n\n", path, message);
}
