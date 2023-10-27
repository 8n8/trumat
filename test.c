#include "trumat.h"
#include <stdio.h>
#include <sys/types.h>
#include <dirent.h>

void run_tests(char*);

int main(int argc, char* argv[]) {
  run_tests("test_input");
}

void run_tests(char* path) {
  DIR* directory;
  struct dirent *item_in_directory;
  directory = opendir(path);
  if (directory != NULL) {
    while ((item_in_directory = readdir(directory)) != NULL) {
      puts(item_in_directory->d_name);
    }
    closedir(directory);
    return;
  } else {
    perror("Couldn't open the directory");
  }
}
