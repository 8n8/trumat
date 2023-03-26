#include "main.h"
#include <stddef.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>

struct Case {
	char* description;
	char* unformatted;
	char* formatted;
};

struct Case cases[] = {
	{
		.description = "hello world formatted",
		.unformatted =
			"module X exposing (x)\n"
			"\n"
			"\n"
			"x =\n"
			"    0\n",
		.formatted =
			"module X exposing (x)\n"
			"\n"
			"\n"
			"x =\n"
			"    0\n"
	},
    {
        .description = "hello world unformatted",
        .unformatted =
            "module X exposing (x)\n"
            "\n"
            "\n"
            "x = 0\n",
        .formatted =
            "module X exposing (x)\n"
            "\n"
            "\n"
            "x =\n"
            "    0\n"
    },
    {
        .description = "oneline list formatted",
        .unformatted =
            "module X exposing (x)\n"
            "\n"
            "\n"
            "x =\n"
            "    [ 1, 2 ]\n",
        .formatted =
            "module X exposing (x)\n"
            "\n"
            "\n"
            "x =\n"
            "    [ 1, 2 ]\n",
    },
    {
        .description = "empty list formatted",
        .unformatted =
            "module X exposing (x)\n"
            "\n"
            "\n"
            "x =\n"
            "    []\n",
        .formatted =
            "module X exposing (x)\n"
            "\n"
            "\n"
            "x =\n"
            "    []\n",
    },
    {
        .description = "multi-line empty list, unformatted",
        .unformatted =
            "module X exposing (x)\n"
            "\n"
            "\n"
            "x =\n"
            "   [\n"
            "\n"
            " ]\n",
        .formatted =
            "module X exposing (x)\n"
            "\n"
            "\n"
            "x =\n"
            "    []\n",
    },
    {
        .description = "multi-line single-item list, formatted",
        .unformatted =
            "module X exposing (x)\n"
            "\n"
            "\n"
            "x =\n"
            "    [ 0, 1\n"
            "    ]\n",
        .formatted =
            "module X exposing (x)\n"
            "\n"
            "\n"
            "x =\n"
            "    [ 0\n"
            "    , 1\n"
            "    ]\n"
    },
	{NULL, NULL, NULL}
};

int string_length(char* s) {
	int i = 0;
	for (; s[i] != '\0'; ++i) {}
	return i;
}

void run_test(struct Case test) {
	printf("%s ", test.description);
	uint8_t* in = malloc(MAX_BUF);
	uint8_t* out = malloc(MAX_BUF);
	int unformatted_len = string_length(test.unformatted);
	memcpy(in, test.unformatted, unformatted_len);
	struct Ast* ast = malloc(sizeof(struct Ast));
	
	int result = format(in, unformatted_len, out, ast);
	
	if (result < 0) {
		printf("FAILED\n    invalid Elm: error code is %d\n", result);
        free(in);
        free(out);
        free(ast);
		return;
	}

    if (result != string_length(test.formatted)) {
        printf("FAILED\n    expecting output of %d bytes, but got %d bytes\n", string_length(test.formatted), result);
        free(in);
        free(out);
        free(ast);
		return;
    }

	for (int i = 0; i < result; ++i) {
		if (out[i] != test.formatted[i]) {
			printf(
				"FAILED\n    not formatted, expecting '%c' at index %d but got '%c'\n",
				test.formatted[i],
				i,
				out[i]);
            free(in);
            free(out);
            free(ast);
			return;
		}
	}
	
    free(in);
    free(out);
    free(ast);
	printf("SUCCESS\n");
}

int main() {
	for (int i = 0; cases[i].description != NULL; ++i) {
		run_test(cases[i]);
	}
}
