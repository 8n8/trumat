#include <stddef.h>
#include "main.h"
#include <dirent.h>

void make_input_file_path(char result[], char* file_name) {
    char* prefix = "test_case/input/";
    int i = 0;
    for (; prefix[i] != '\0'; ++i) {
        result[i] = prefix[i];
    }

    int j = 0;
    for (; file_name[j] != '\0'; ++j) {
        result[i+j] = file_name[j];
    }

    result[i+j] = '\0';
}

void make_expected_file_path(char result[], char* file_name) {
    char* prefix = "test_case/expected/";
    int i = 0;
    for (; prefix[i] != '\0'; ++i) {
        result[i] = prefix[i];
    }

    int j = 0;
    for (; file_name[j] != '\0'; ++j) {
        result[i+j] = file_name[j];
    }

    result[i+j] = '\0';
}

char* got_path = ".temp_trumat_path";

int compare_got_and_expected(char* test_file_name) {
    char expected_file_path[300];
    make_expected_file_path(expected_file_path, test_file_name);

    FILE* expected_file = fopen(expected_file_path, "rb");
    if (expected_file == NULL) {
        printf("FAILURE\ncouldn't open expected file: %s\n", expected_file_path);
        return -1;
    }

    FILE* got_file = fopen(got_path, "rb");
    if (got_file == NULL) {
        printf("FAILURE\ncouldn't open got file: %s\n", got_path);
        return -1;
    }

    int got, expected;
    for (int i = 0; i < 100 ; ++i) {
        got = fgetc(got_file);
        expected = fgetc(expected_file);
        if (got == EOF && expected != EOF) {
            printf("FAILURE\nreached end of result file but not expected file\n");
            return -1;
        }
        if (got != EOF && expected == EOF) {
            printf("FAILURE\nreached end of expected file but not result file\n");
            return -1;
        }
        if (got == EOF && expected == EOF) {
            break;
        }
        if (got != expected) {
            printf(
                "FAILURE\ninvalid character %c at position %d, expecting %c "
                "in file %s\n",
                got,
                i,
                expected,
                test_file_name);
            return -1;
        }
    }

    int close_expected_result = fclose(expected_file);
    if (close_expected_result != 0) {
        printf("FAILURE\ncouldn't close expected file: %s\n", expected_file_path);
        return -1;
    }

    return 0;
}

int one_test(char* test_file_name) {
    printf("%s ", test_file_name);

    char input_file_path[300];
    make_input_file_path(input_file_path, test_file_name);

    FILE* input_file = fopen(input_file_path, "rb");
    if (input_file == NULL) {
        printf("FAILURE\ncouldn't open input file: %s\n", input_file_path);
        return -1;
    }

    FILE* got_file = fopen(got_path, "wb");
    if (got_file == NULL) {
        printf("FAILURE\ncouldn't open got file: %s\n", got_path);
        return -1;
    }

    int format_result = format(input_file, got_file);
    if (format_result != 0) {
        printf(
            "FAILURE\ninvalid Elm in %s\nerror code: %d\n",
            input_file_path,
            format_result);
        return -1;
    }

    int close_got_result = fclose(got_file);
    if (close_got_result != 0) {
        printf("FAILURE\ncouldn't close temporary file: %s\n", got_path);
        return -1;
    }

    int close_input_result = fclose(input_file); 
    if (close_input_result != 0) {
        printf("FAILURE\ncouldn't close input file: %s\n", input_file_path);
        return -1;
    }

    int test_result = compare_got_and_expected(test_file_name);
    if (test_result != 0) {
        return test_result;
    }

    close_got_result = fclose(got_file);
    if (close_got_result != 0) {
        printf("FAILURE\ncouldn't close temporary file: %s\n", got_path);
        return -1;
    }

    printf("SUCCESS\n");

    return 0;
}

int main() {
    DIR* input_directory = opendir("test_case/input");
    if (input_directory == NULL) {
        return -1;
    }

    for (
        struct dirent* directory_entry = readdir(input_directory);
        directory_entry != NULL;
        directory_entry = readdir(input_directory)) {

        if (directory_entry->d_name[0] == '.') {
            continue;
        }
        int result = one_test(directory_entry->d_name);
        if (result != 0) {
            return -1;
        }
    }

    return 0;
}
