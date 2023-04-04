#include <stddef.h>
#include <stdio.h>
#include "main.h"

struct Case {
    char* description;
    char* in;
    char* out;
};

int string_equal(char* a, char* b) {
    for (; *a == *b && *a != '\0'; ++a && ++b) {
    }

    return *a == *b;
}

void one_test(struct Case test_case, char* out) {
    printf("%s ", test_case.description);
    int result = format(test_case.in, out);

    if (result < 0) {
        printf("invalid Elm FAILURE\n");
        return;
    }

    if (!string_equal(test_case.out, out)) {
        printf("FAILURE\n");
        printf("EXPECTING:\n%s\nGOT:\n%s\n", test_case.out, out);
        return;
    }

    puts("SUCCESS\n");
}

struct Case cases[] = {
    { .description = "hello world formatted",
      .in =
        "module X exposing (x)\n"
        "\n"
        "\n"
        "x =\n"
        "    0\n",
      .out =
        "module X exposing (x)\n"
        "\n"
        "\n"
        "x =\n"
        "    0\n",
    },
    { NULL, NULL, NULL }
};

int main() {
    char in[10000];
    for (int i = 0; cases[i].description != NULL; ++i) {
        one_test(cases[i], in);
    }
}
