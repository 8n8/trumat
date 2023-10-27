#include "trumat.h"
#include <dirent.h>
#include <stdio.h>
#include <sys/types.h>

void run_tests(char *);

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
  } else {
    perror("Couldn't open the directory");
  }
}
