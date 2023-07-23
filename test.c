#include "trumat.h"
#include <dirent.h>


void one_test_help(
    FILE* input_file,
    FILE* expected_file,
    FILE* output_file) {

    int result = format(input_file, output_file);

    if (result != 0) {
        printf(" FAIL invalid Elm\n");
        return;
    }

    fseek(output_file, 0, SEEK_SET);

    for (int i = 0; i < 1000; ++i) {
        char expected_char = fgetc(expected_file);
        char got_char = fgetc(output_file);

        if (expected_char == EOF && got_char == EOF) {
            printf(" SUCCESS\n");
            return;
        }

        if (expected_char != got_char) {
            printf(
                " FAIL expected %c, got %c at position %d\n",
                expected_char,
                got_char,
                i);
            return;
        }
    }
}

void make_input_path(char* file_name, char input_path[256]) {
    char* directory = "testInput/";
    int i = 0;
    for (; directory[i] != '\0'; ++i) {
        input_path[i] = directory[i];
    }

    for (int j = 0; file_name[j] != '\0'; ++j) {
        input_path[i] = file_name[j];
        ++i;
    }

    input_path[i] = '\0';
}

void make_expected_path(char* file_name, char input_path[256]) {
    char* directory = "testExpected/";
    int i = 0;
    for (; directory[i] != '\0'; ++i) {
        input_path[i] = directory[i];
    }

    for (int j = 0; file_name[j] != '\0'; ++j) {
        input_path[i] = file_name[j];
        ++i;
    }

    input_path[i] = '\0';
}

void one_test(char* file_name, struct Memory* memory) {
    printf("%s", file_name);

    char input_path[256];
    char expected_path[256];
    make_input_path(file_name, input_path);
    make_expected_path(file_name, expected_path);

    FILE* input_file = fopen(input_path, "r");
    FILE* expected_file = fopen(expected_path, "r");
    FILE* output_file = fopen("temporaryTestingFile.elm", "w+");

    zero_memory(memory);

    one_test_help(input_file, expected_file, output_file);

    fclose(input_file);
    fclose(expected_file);
    fclose(output_file);
    remove("temporaryTestingFile.elm");
}


struct Memory MEMORY;

int main(int argc, char* argv[]) {
    DIR* parent;
    struct dirent* child;
    parent = opendir("testInput");
    if (parent) {
        while ((child = readdir(parent)) != NULL) {
            char* testFileName = child->d_name;
            if (testFileName[0] == '.') {
                continue;
            }

            one_test(testFileName, &MEMORY);
        }
        closedir(parent);
    }
    return(0);
}
