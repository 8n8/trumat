set -e

gcc -g3 -c -o main.o main.c -Wall -Werror
gcc -g3 test.c main.o -Wall -Werror
./a.out

