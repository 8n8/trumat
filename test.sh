gcc -g3 -c main.c -o main.o -Wall
gcc -g3 test.c main.o -o test -Wall
./test
