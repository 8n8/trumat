#include <stdio.h>

const char *usage = "expecting two arguments:\n"
                    "1. --overwrite to confirm it's OK to recursively "
                    "overwrite all the Elm files in the path\n"
                    "2. the path to format\n"
                    "\n"
                    "for example:\n"
                    "  $ trumat --overwrite Hello.elm\n"
                    "  $ trumat --overwrite some/directory/name\n";

int string_equal(char *a, char *b) {
  for (; *a == *b && *a != '\0'; ++a, ++b) {
  }
  return *a == '\0' && *b == '\0';
}

int main(int argc, char *argv[]) {
  if (argc != 3) {
    fputs(usage, stderr);
    return -1;
  }

  if (!string_equal(argv[1], "--overwrite")) {
    fputs(usage, stderr);
    return -1;
  }

  return 0;
}
