#include <dirent.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>

// Twice the size of the largest Elm file I have seen.
#define MAX_SRC 1400 * 1000
static uint8_t SRC[MAX_SRC];
static uint16_t ROW[MAX_SRC];
static uint16_t COLUMN[MAX_SRC];
static int NUM_SRC = 0;
static int I = -1;
static FILE *OUT;

// The formatter works on top level definitions, so this state should be
// enough to contain everything that is needed to format the largest top
// level definition.
//
// Basic assumptions:
//
// - max 5000 lines of code in a single top-level definition
// - average 60 bytes per line of code
// - average 5 nodes per line

void dbg_src() {
  for (int i = 0; i < NUM_SRC; ++i) {
    if (SRC[i] == '\n') {
      fputs("\\n", stdout);
      continue;
    }
    fputc(SRC[i], stdout);
  }
  fputc('\n', stdout);
}

enum node_type {
  NO_DOCS_NODE = 1,
  EMPTY_BLOCK_COMMENT_NODE,
  SINGLE_LINE_BLOCK_COMMENT_NODE,
  MODULE_EXPOSE_ALL_VARIANTS_NODE,
  COMPACT_BLOCK_COMMENT_NODE,
};

static const char *usage = "expecting two arguments:\n"
                           "1. --overwrite to confirm it's OK to recursively "
                           "overwrite all the Elm files in the path\n"
                           "2. the path to format\n"
                           "\n"
                           "for example:\n"
                           "  $ trumat --overwrite Hello.elm\n"
                           "  $ trumat --overwrite some/directory/name\n";

static int string_equal(char *a, char *b) {
  for (; *a == *b && *a != '\0'; ++a, ++b) {
  }
  return *a == '\0' && *b == '\0';
}

static int with_out_file() {
  I = -1;
  fputs("module X exposing (x)\n\n\nx =\n    0\n", OUT);
  return 0;
}

static void calculate_row_numbers() {
  int row = 0;
  for (int i = 0; i < NUM_SRC; ++i) {
    ROW[i] = row;
    if (SRC[i] == '\n') {
      ++row;
    }
  }
}

static void calculate_column_numbers() {
  int column = 0;
  for (int i = 0; i < NUM_SRC; ++i) {
    COLUMN[i] = column;
    if (SRC[i] == '\n') {
      column = 0;
    } else {
      ++column;
    }
  }
}

static int read_src(char *path) {
  FILE *file = fopen(path, "r");
  if (file == NULL) {
    return -1;
  }
  NUM_SRC = fread(SRC, 1, MAX_SRC, file);
  fclose(file);
  if (NUM_SRC == MAX_SRC) {
    fprintf(stderr, "file too large: %s, maximum size is %d bytes\n", path,
            MAX_SRC);
    return -1;
  }
  return 0;
}

static void format_file(char *path) {
  if (read_src(path)) {
    return;
  }

  calculate_row_numbers();
  calculate_column_numbers();

  OUT = fopen(path, "w");
  if (OUT == NULL) {
    return;
  }
  const int result = with_out_file();
  fclose(OUT);
  if (result) {
    fprintf(stderr, "could not format %s\n", path);
  }
}

static int string_length(char *s) {
  int n = 0;
  for (; *s != '\0'; ++s) {
    ++n;
  }
  return n;
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

static void format_src(char *path) {
  DIR *directory_handle = opendir(path);
  if (directory_handle == NULL && !is_elm_path(path)) {
    return;
  }

  if (directory_handle == NULL && is_elm_path(path)) {
    format_file(path);
    return;
  }

  struct dirent *entry = readdir(directory_handle);
  while (entry != NULL) {
    if (entry->d_name[0] != '.') {
      char sub_path[256];
      make_sub_path(path, entry->d_name, sub_path);
      format_src(sub_path);
    }
    entry = readdir(directory_handle);
  }
  closedir(directory_handle);
}

int main(int argc, char *argv[]) {
  if (argc != 3 || !string_equal(argv[1], "--overwrite")) {
    fputs(usage, stderr);
    return -1;
  }

  format_src(argv[2]);

  return 0;
}
