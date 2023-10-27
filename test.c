#include "trumat.h"
#include <stdio.h>
#include <sys/types.h>
#include <dirent.h>


int main(int argc, char* argv[]) {
  DIR* dp;
  struct dirent *ep;
  dp = opendir("./");
  if (dp != NULL) {
    while ((ep = readdir(dp)) != NULL) {
      puts(ep->d_name);
    }
    (void) closedir(dp);
    return 0;
  } else {
    perror("Couldn't open the directory");
    return -1;
  }
}
