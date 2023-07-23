#include "trumat.h"

void zero_memory(struct Memory* memory) {
}

int format(FILE* input_file, FILE* output_file) {
    while (1) {
        int input_char = fgetc(input_file);
        if (input_char == EOF) {
            return 0;
        }

        fputc(input_char, output_file);
    }
}
