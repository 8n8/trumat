#include <stddef.h>
#include "trumat.h"
#include <stdio.h>

struct TestCase {
    char* description;
    char* unformatted;
    char* formatted;
};

void run_test(struct TestCase testCase, char in[BIG], char out[BIG]) {
    printf("%s ", testCase.description);

    for (int i = 0; testCase.unformatted[i] != '\0'; ++i) {
        in[i] = testCase.unformatted[i];
    }

    int result = format(in, out);
    if (result != 0) {
        printf("FAILED\n    error code: %d\n", result);
        return;
    }

    for (int i = 0; testCase.formatted[i] != 0; ++i) {
        if (testCase.formatted[i] != out[i]) {
            printf(
                "FAILED\n    expecting '%c' at position %d but got '%c'\n",
                testCase.formatted[i],
                i,
                out[i]);
            return;
        }
    }

    printf("SUCCESS\n");
}

struct TestCase CASES[] = {
    {
        .description = "hello world formatted",
        .unformatted =
            "module X exposing (x)\n"
            "\n"
            "\n"
            "x =\n"
            "    [0]\n",
        .formatted =
            "module X exposing (x)\n"
            "\n"
            "\n"
            "x =\n"
            "    [ 0 ]\n",
    },
    {NULL, NULL, NULL},
};

void zero_buf(char buf[BIG]) {
    for (int i = 0; i < BIG; ++i) {
        buf[i] = 0;
    }
}

char IN[BIG];
char OUT[BIG];

int main(int argc, char* argv[]) {
    for (int i = 0; CASES[i].description != NULL; ++i) {
        zero_buf(IN);
        zero_buf(OUT);

        run_test(CASES[i], IN, OUT);
    }

    return 0;
}
