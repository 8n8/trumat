#include "trumat.h"
#include <dirent.h>
#include <stdio.h>
#include <sys/types.h>

void check_unchanged(char *, char in[CODE_SIZE], char out[CODE_SIZE]);
void run_positive_tests(char *, char in[CODE_SIZE], char out[CODE_SIZE],
                        struct memory *);
void run_no_change_tests(char *, char in[CODE_SIZE], char out[CODE_SIZE],
                         struct memory *);
void run_one_positive_test(char *, char in[CODE_SIZE], char out[CODE_SIZE],
                           struct memory *);
void run_one_no_change_test(char *, char in[CODE_SIZE], char out[CODE_SIZE],
                            struct memory *);
void make_expected_path(char *, char *);
void print_error(char *, char *);
void check_expected(char *, char out[CODE_SIZE]);

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

char IN[CODE_SIZE];
char OUT[CODE_SIZE];
struct memory MEMORY;
int NUM_PASSED = 0;
int NUM_IGNORED = 0;

int main(int argc, char *argv[]) {
  run_positive_tests("test_input", IN, OUT, &MEMORY);
  zero_memory(&MEMORY);
  run_no_change_tests("test_formatted", IN, OUT, &MEMORY);
  printf("%d tests passed\n", NUM_PASSED);
  printf("%d tests successfully ignored\n", NUM_IGNORED);
}

void run_no_change_tests(char *path, char in[CODE_SIZE], char out[CODE_SIZE],
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

void run_positive_tests(char *path, char in[CODE_SIZE], char out[CODE_SIZE],
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

void run_one_no_change_test(char *in_path, char in[CODE_SIZE],
                            char out[CODE_SIZE], struct memory *memory) {
  FILE *in_file = fopen(in_path, "rb");
  if (in_file == NULL) {
    char error_message[256];
    sprintf(error_message, "could not open file: %s", in_path);
    print_error(in_path, error_message);
    return;
  }
  int in_size = fread(in, 1, CODE_SIZE, in_file);
  in[in_size] = 0;
  fclose(in_file);

  int result = format(in, out, memory);

  if (result != 0) {
    ++NUM_IGNORED;
    return;
  }

  check_unchanged(in_path, in, out);
}

void check_unchanged(char *in_path, char in[CODE_SIZE], char out[CODE_SIZE]) {
  for (int i = 0; i < CODE_SIZE; ++i) {
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

  ++NUM_PASSED;
}

void run_one_positive_test(char *in_path, char in[CODE_SIZE],
                           char out[CODE_SIZE], struct memory *memory) {
  FILE *in_file = fopen(in_path, "rb");
  if (in_file == NULL) {
    char error_message[256];
    sprintf(error_message, "could not open file: %s", in_path);
    print_error(in_path, error_message);
    return;
  }
  int in_size = fread(in, 1, CODE_SIZE, in_file);
  in[in_size] = 0;
  fclose(in_file);

  int result = format(in, out, memory);

  if (result != 0) {
    char error_message[256];
    sprintf(error_message, "formatter failed with non-zero result: %d", result);
    print_error(in_path, error_message);
    return;
  }

  check_expected(in_path, out);
}

void check_expected(char *in_path, char out[CODE_SIZE]) {
  char expected_path[256];
  make_expected_path(in_path, expected_path);

  FILE *expected_file = fopen(expected_path, "rb");
  if (expected_file == NULL) {
    char error_message[300];
    sprintf(error_message, "could not open file; %s", expected_path);
    print_error(expected_path, error_message);
    return;
  }

  for (int i = 0; i < CODE_SIZE; ++i) {
    int expected = fgetc(expected_file);
    if (expected == EOF && out[i] == 0) {
      break;
    }

    if (expected != out[i]) {
      char error_message[300];
      sprintf(error_message, "expected '%c' but got '%c' at position %d",
              expected, out[i], i);
      print_error(in_path, error_message);
      fclose(expected_file);
      return;
    }
  }
  fclose(expected_file);

  ++NUM_PASSED;
}

void print_error(char *path, char *message) {
  printf("FAILED: %s: %s\n", path, message);
}
