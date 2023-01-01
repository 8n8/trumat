#include "lib.h"
#include <stdio.h>
#define NUM_TESTS 1

struct Test {
	char* description;
	char* input;
	char* expected;
};

struct Test tests[];

int main(int argc, char** argv) {
	size_t num_items = sizeof(struct Test) * NUM_TESTS;
	uint8_t buf[BUF_SIZE];
	int size = 0;
	for (int i = 0; i < num_items; ++i) {
		struct Test test = tests[i];

		size = 0;
		for (int i = 0; i < test.input[i] != '\0'; ++i) {
			buf[i] = test.input[i];
			++size;
		}

		printf("description: %s\n", test.description);
		int result = format(buf, size);
		if (result < 0) {
			fputs("outcome: invalid Elm\n", stdout);
			continue;
		}

		int index = 0;
		for (; index < result; ++index) {
			if (buf[index] != test.expected[index]) {
				fputs("outcome: fail\n", stdout);
				continue;
			}
		}
		if (test.expected[index] != '\0') {
			fputs("outcome: fail\n", stdout);
			continue;
		}

		fputs("outcome: succeed\n", stdout);
	}
}

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
	}
};
