set -e

gcc -g3 -c -o main.o main.c -Wall
gcc -g3 test.c main.o -Wall
./a.out

