set -e

gcc -c -o lib.o lib.c
gcc -c -o test.o test.c
gcc lib.o test.c
./a.out
