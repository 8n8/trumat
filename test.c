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
	},
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
	}
};

uint8_t buf[BUF_SIZE];

int main(int argc, char** argv) {
	size_t num_items = sizeof(struct Test) * NUM_TESTS;
	int size = 0;
	for (int i = 0; i < NUM_TESTS; ++i) {
		struct Test test = tests[i];

		size = 0;
		for (int j = 0; test.input[j] != '\0'; ++j) {
			buf[j] = test.input[j];
			++size;
		}

		printf("description: %s\n", test.description);
		int result = format(buf, size);
		printf("result: %d\n", result);
		if (result < 0) {
			fputs("outcome: invalid Elm\n", stdout);
			continue;
		}

		int index = 0;
		for (; index < result; ++index) {
			if (buf[index] != test.expected[index]) {
				fputs("outcome: fail\n", stdout);
				printf("expected: %s\n", test.expected);
				printf("     got: %s\n", buf);
				return 0;
			}
		}
		if (test.expected[index] != '\0') {
			fputs("outcome: fail\n", stdout);
			continue;
		}

		fputs("outcome: succeed\n\n", stdout);
	}
}
