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
	{NULL, NULL, NULL}
};

int string_length(char* s) {
	int i = 0;
	for (; s[i] != '\0'; ++i) {}
	return i;
}

void run_test(struct Case test) {
	printf("%s ", test.description);
	uint8_t* text = malloc(MAX_BUF);
	int unformatted_len = string_length(test.unformatted);
	memcpy(text, test.unformatted, unformatted_len);
	struct Ast* ast = malloc(sizeof(struct Ast));
	
	int result = format(text, unformatted_len, ast);
	
	if (result < 0) {
		printf("FAILED\n    invalid Elm: error code is %d\n", result);
		return;
	}
	
	for (int i = 0; i < result; ++i) {
		if (text[i] != test.formatted[i]) {
			printf(
				"FAILED\n    not formatted, expecting %c at index %d but got %c\n",
				test.formatted[i],
				i,
				text[i]);
			return;
		}
	}
	
	printf("SUCCESS\n");
}

int main() {
	for (int i = 0; cases[i].description != NULL; ++i) {
		run_test(cases[i]);
	}
}
