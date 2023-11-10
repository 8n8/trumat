#include "trumat.h"
#include <dirent.h>
#include <stdio.h>
#include <sys/types.h>

static char *test_only = "";

static int is_excluded_by_only(char *path) {
  if (test_only[0] == '\0') {
    return 0;
  }

  return !string_equal(path, test_only);
}

static void print_error(char *path, char *message) {
  fprintf(stderr, "FAILED: %s: %s\n", path, message);
}

static void make_expected_path(char *in_path, char *expected_path) {
  char *expected_root = "test_data/expected/";
  int expected_i = 0;
  for (; expected_root[expected_i] != 0; ++expected_i) {
    expected_path[expected_i] = expected_root[expected_i];
  }

  char *in_root = "test_data/input/";
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

static struct text_memory MEMORY;
static int NUM_PASSED = 0;
static int NUM_IGNORED = 0;

static void check_unchanged(char *in_path, struct text in, struct text out,
                            struct text_memory *m) {
  if (text_length(in) != text_length(out)) {
    char error_message[300];
    sprintf(error_message,
            "expected equal length texts but got input length of %d and output "
            "length of %d",
            text_length(in), text_length(out));
    print_error(in_path, error_message);
    return;
  }
  for (int i = 0;; ++i) {
    int in_result = text_index(in, i, m);
    int out_result = text_index(out, i, m);
    if (in_result < 0 && out_result < 0) {
      break;
    }

    if (in_result != out_result) {
      char error_message[300];
      sprintf(error_message, "expected '%c' but got '%c' at position %d",
              in_result, out_result, i);
      print_error(in_path, error_message);
      return;
    }
  }

  ++NUM_PASSED;
}

static void run_one_no_change_test(char *in_path, struct text_memory *m) {
  FILE *in_file = fopen(in_path, "rb");
  if (in_file == NULL) {
    char error_message[256];
    sprintf(error_message, "could not open file: %s", in_path);
    print_error(in_path, error_message);
    return;
  }
  struct text in;
  int result = text_from_file(in_file, &in, m);
  fclose(in_file);
  if (result) {
    char error_message[256];
    sprintf(error_message, "could not read the file: %s", in_path);
    print_error(in_path, error_message);
    return;
  }

  struct text out;
  result = format(in, &out, m);
  if (result != 0) {
    ++NUM_IGNORED;
    return;
  }

  check_unchanged(in_path, in, out, m);
}

static void run_no_change_tests(char *path, struct text_memory *memory) {
  DIR *directory = opendir(path);
  struct dirent *item_in_directory;
  if (directory != NULL) {
    item_in_directory = readdir(directory);
    while (item_in_directory != NULL) {
      if (!is_dot_path(item_in_directory->d_name)) {
        char sub_path[256];
        make_sub_path(path, item_in_directory->d_name, sub_path);
        run_no_change_tests(sub_path, memory);
      }
      item_in_directory = readdir(directory);
    }
    closedir(directory);
    return;
  }

  if (!is_elm_path(path)) {
    return;
  }

  if (is_excluded_by_only(path)) {
    return;
  }

  text_zero_memory(memory);
  run_one_no_change_test(path, memory);
}

static void check_expected(char *in_path, struct text out,
                           struct text_memory *m) {
  char expected_path[256];
  make_expected_path(in_path, expected_path);

  FILE *expected_file = fopen(expected_path, "rb");
  if (expected_file == NULL) {
    char error_message[300];
    sprintf(error_message, "could not open file; %s", expected_path);
    print_error(expected_path, error_message);
    return;
  }

  for (int i = 0;; ++i) {
    int expected = fgetc(expected_file);
    if (expected == EOF && text_index(out, i, m) < 0) {
      break;
    }

    if (expected != text_index(out, i, m)) {
      char error_message[300];
      sprintf(error_message, "expected '%c' but got '%c' at position %d",
              expected, text_index(out, i, m), i);
      print_error(in_path, error_message);
      fclose(expected_file);
      return;
    }
  }
  fclose(expected_file);

  ++NUM_PASSED;
}

static void run_one_positive_test(char *in_path, struct text_memory *m) {
  FILE *in_file = fopen(in_path, "rb");
  if (in_file == NULL) {
    char error_message[256];
    sprintf(error_message, "could not open file: %s", in_path);
    print_error(in_path, error_message);
    return;
  }

  struct text in;
  int result = text_from_file(in_file, &in, m);
  fclose(in_file);
  if (result) {
    char error_message[256];
    sprintf(error_message, "could not read the file: %s", in_path);
    print_error(in_path, error_message);
    return;
  }

  struct text out;
  result = format(in, &out, m);

  if (result != 0) {
    char error_message[256];
    sprintf(error_message, "formatter failed with non-zero result: %d", result);
    print_error(in_path, error_message);
    return;
  }

  check_expected(in_path, out, m);
}

static void run_positive_tests(char *path, struct text_memory *memory) {
  DIR *directory = opendir(path);
  struct dirent *item_in_directory;
  if (directory != NULL) {
    item_in_directory = readdir(directory);
    while (item_in_directory != NULL) {
      if (!is_dot_path(item_in_directory->d_name)) {
        char sub_path[256];
        make_sub_path(path, item_in_directory->d_name, sub_path);
        run_positive_tests(sub_path, memory);
      }
      item_in_directory = readdir(directory);
    }
    closedir(directory);
    return;
  }

  if (!is_elm_path(path)) {
    return;
  }

  if (is_excluded_by_only(path)) {
    return;
  }

  text_zero_memory(memory);
  run_one_positive_test(path, memory);
}

int main(int argc, char *argv[]) {
  run_positive_tests("test_data/input", &MEMORY);
  run_no_change_tests("test_data/dont_change", &MEMORY);
  printf("%d tests passed\n", NUM_PASSED);
  printf("%d tests successfully ignored\n", NUM_IGNORED);
}
