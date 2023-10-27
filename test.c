#include "trumat.h"
#include <stdio.h>
#include <sys/types.h>
#include <dirent.h>


int main(int argc, char* argv[]) {
  DIR* directory;
  struct dirent *item_in_directory;
  directory = opendir("./");
  if (directory != NULL) {
    while ((item_in_directory = readdir(directory)) != NULL) {
      puts(item_in_directory->d_name);
    }
    closedir(directory);
    return 0;
  } else {
    perror("Couldn't open the directory");
    return -1;
  }
}
