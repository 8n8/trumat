#include "lib.h"
#include <stdio.h>
#define NUM_TESTS 2

struct Test {
	char* description;
	char* input;
	char* expected;
};

struct Test tests[NUM_TESTS] = {
	{
	.description = "Single trailing whitespace",
 	.input =
		"module X exposing (x)\n"
		"\n"
		"\n"
		"x = \n"
		"    0\n",
	.expected =
		"module X exposing (x)\n"
		"\n"
		"\n"
		"x =\n"
		"    0\n"
	},
	{
	.description = "Simple formatted, so don't change",
 	.input =
		"module X exposing (x)\n"
		"\n"
		"\n"
		"x =\n"
		"    0\n",
	.expected =
		"module X exposing (x)\n"
		"\n"
		"\n"
		"x =\n"
		"    0\n"
	}
};

struct Buf buf;
struct Ast ast;


void one_test(struct Test* test) {
	init_ast(&ast);
	init_buf(&buf);
	for (int j = 0; test->input[j] != '\0'; ++j) {
		buf.chars[j] = test->input[j];
		++buf.size;
	}

	printf("======================================================\ndescription: %s\n", test->description);
	int result = format(&buf, &ast);
	if (result != 0) {
		fputs("outcome: invalid Elm\n", stdout);
		return;
	}

	int index = 0;
	for (; index < buf.size; ++index) {
		if (buf.chars[index] != test->expected[index]) {
			fputs("outcome: fail\n", stdout);
			printf("EXPECTED\n%s\n", test->expected);
			printf("GOT     \n%s\n", buf.chars);
			return;
		}
	}
	if (test->expected[index] != '\0') {
		fputs("outcome: fail\n", stdout);
		return;
	}

	fputs("outcome: succeed\n\n", stdout);
}

int main(int argc, char** argv) {
	for (int i = 0; i < NUM_TESTS; ++i) {
		one_test(&tests[i]);
	}
}
