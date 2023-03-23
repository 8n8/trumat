set -e

gcc -g3 -c -o main.o main.c
gcc -g3 test.c main.o
./a.out

