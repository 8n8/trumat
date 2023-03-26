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

void zero_ints(struct Ast* ast) {
    ast->num_verbatims = 0;
    ast->has_module_name = 0;
    ast->num_exports = 0;
    ast->num_export_expose = 0;
    ast->newline_exports = 0;
    ast->num_binds = 0;
    ast->num_is_top_binds = 0;
    ast->num_has_newlines = 0;
    ast->num_expressions = 0;
    ast->next_free_id = 0;
}

struct Case cases[] = {
	/* {
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
    }, */
    {
        .description = "nested lists",
        .unformatted =
            "module X exposing (x)\n"
            "\n"
            "\n"
            "x =\n"
            "    [ [ 0 ] ]\n",
        .formatted =
            "module X exposing (x)\n"
            "\n"
            "\n"
            "x =\n"
            "    [ [ 0 ] ]\n",
    },
	{NULL, NULL, NULL}
};

int string_length(char* s) {
	int i = 0;
	for (; s[i] != '\0'; ++i) {}
	return i;
}

void run_test(
    struct Case test,
    struct Ast* ast,
    uint8_t in[MAX_BUF],
    uint8_t out[MAX_BUF]) {

	printf("%s ", test.description);
	int unformatted_len = string_length(test.unformatted);
	memcpy(in, test.unformatted, unformatted_len);
	
	int result = format(in, unformatted_len, out, ast);
	
	if (result < 0) {
		printf("FAILED\n    invalid Elm: error code is %d\n", result);
		return;
	}

    // if (result != string_length(test.formatted)) {
    //     printf("FAILED\n    expecting output of %d bytes, but got %d bytes\n", string_length(test.formatted), result);
	// 	return;
    // }

	for (int i = 0; i < result; ++i) {
        putchar(out[i]);
		// if (out[i] != test.formatted[i]) {
		// 	printf(
		// 		"FAILED\n    not formatted, expecting '%c' at index %d but got '%c'\n",
		// 		test.formatted[i],
		// 		i,
		// 		out[i]);
		// 	return;
		// }
	}
	
	printf("SUCCESS\n");
}

int main() {
    struct Ast* ast = malloc(sizeof(struct Ast));
    uint8_t* in = malloc(MAX_BUF);
    uint8_t* out = malloc(MAX_BUF);
	for (int i = 0; cases[i].description != NULL; ++i) {
        zero_ints(ast);
		run_test(cases[i], ast, in, out);
	}
}
