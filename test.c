#include "trumat.h"
#include <dirent.h>
#include <stdio.h>
#include <sys/types.h>

void run_tests(char *);
void run_one_test(char *);
int is_elm_path(char *);

int main(int argc, char *argv[]) { run_tests("test_input"); }

void run_tests(char *path) {
  DIR *directory = opendir(path);
  struct dirent *item_in_directory;
  if (directory != NULL) {
    item_in_directory = readdir(directory);
    while (item_in_directory != NULL) {
      puts(item_in_directory->d_name);
      item_in_directory = readdir(directory);
    }
    closedir(directory);
    return;
  }

  if (!is_elm_path(path)) {
    return;
  }

  run_one_test(path);
}
