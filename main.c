#include <dirent.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>

// Some Elm stats from elm-pages:
//
// - 375 files
// - 65k lines of code
// - 3.9 MB of code
//
// - average 60 bytes per line of code
// - average 10 kB per file
//
// Should be enough to fit even a massive codebase. The largest Elm codebase
// I have heard of has about 700000 lines of code.
//
// Let's make this tool able to handle double that. So 1.4 million lines of
// code. If each line of code is average 60 bytes, that's 84 MB.
#define MAX_SRC 84 * 1000 * 1000
uint8_t SRC[MAX_SRC]; // 84 MB

// With average of 10kB per file that is a maximum of 8400 files.
#define MAX_FILES 8400
uint32_t FILE_END[MAX_FILES]; // 269 KB
// Say there are 256 bytes per file path on average. That's 2.2 MB.
#define MAX_PATHS 2200 * 1000
uint8_t PATH[MAX_PATHS]; // 2.2 MB
uint32_t PATH_END[MAX_FILES]; // 34 KB
int NUM_FILES = 0;


// The tokens are mostly single characters. Say every 5th token is multi
// character. That means 5 tokens will be 9 bytes, so say 2 bytes per token
// on average. So let's assume half as many tokens as source code bytes.
#define MAX_TOKENS 42 * 1000 * 1000
uint8_t TOKENS[MAX_TOKENS]; // 42 MB
int NUM_TOKENS = 0;

#define MAX_TEXT_TOKENS 8000 * 1000
// Token IDs that have text, such as variable names or comments. The token
// IDs are the array indices.
uint32_t TEXT_TOKENS[MAX_TEXT_TOKENS]; // 32 MB
uint32_t TEXT_TOKEN_START[MAX_TEXT_TOKENS]; // 32 MB
uint16_t TEXT_TOKEN_SIZE[MAX_TEXT_TOKENS]; // 16 MB
int NUM_TEXT_TOKENS = 0;

// Let's say there are three nodes per line of code on average. That's
// 4.2 million nodes.
#define MAX_NODES 4200 * 1000

// The largest Elm file I have ever found is 700 kB. I think it was in
// elm-review. Let's say I need to be able to format a file that's 2 MB.
//
// Say there are 100 bytes per line on average. That's 20000 lines of code.
//
// Say there is an average of 3 nodes per line, that's 60000 nodes. That
// fits in 16 bits.
#define MAX_NODES 256 * 256
uint16_t PARENT[MAX_NODES]; // 130 KB
uint16_t NEXT_NODE = 0;

// Quite a lot of the nodes will have text attached to them, such as
// variable names or comments. Say the average text is 6 bytes and that half
// the nodes have text. That's 200 kB.
#define MAX_TEXT 200 * 1000
uint8_t TEXT[MAX_TEXT]; // 200 KB
uint32_t TEXT_END[MAX_NODES]; // 362 KB

// This contains all the source code of all the Elm files. It contains all
// the Elm modules one after another.
uint8_t SRC[SRC_SIZE];

// Using the logic for calculating the source size above, the maximum number
// of files is 4 * 1900 = 7600.
#define MAX_FILES 7600
uint32_t FILE_END[MAX_FILES]; // 30 KB
int NUM_FILES = 0;

// Each file has a path. It's unlikely the average path is more than 256
// bytes. If there are maximum 7600 files, that's 1.9 MB.
#define MAX_PATHS 1900 * 1000
uint8_t PATH[MAX_PATHS];
uint16_t PATH_END[MAX_FILES];

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
  SRC[*src_index] = '\1';
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
