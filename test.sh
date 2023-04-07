gcc -g3 -c main.c -o main.o
gcc -g3 test.c main.o -o test
./test
