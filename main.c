#include <stdio.h>
#include <stdint.h>

enum ErrorCode {
	NoElmDotJson,
};

struct Src {
	// Assume that a module has no more than 100,000 lines,
	// and that the average line length is 50 chars. So
	// that is 5,000,000 bytes maximum for a source file.
	uint8_t src[5000000];
	uint32_t num_src;
}

struct Db {
	struct Src src;
	struct Tokens tokens;
	struct Ast ast;
}

struct Tokens {
	// Assume that there are on average 3 characters per
	// token, that is 5,000,000 / 3 = 1,666,666
	uint8_t type[1666666];
	uint32_t start[1666666];
	uint32_t end[1666666];
	uint32_t num;
}

struct BlockComment {
	// Say there is a block comment every 10 lines of code, and
	// 100,000 lines in total, that is 10,000 block comments.
	uint32_t start[10000];
	uint32_t end[10000];
	uint16_t num;
}

struct LineComment {
	// Say there are twice as many line comments as block comments,
	// that is 20,000.
	uint32_t start[20000];
	uint32_t end[20000];
	uint16_t num;
}

struct Ordering {
	// The order things appear in the source file. This is needed so
	// that order is preserved when the output is written. Assume that
	// there are three AST items per line of code, and there are
	// 100,000 lines of code. That is 300,000 items maximum.
	uint32_t id[300000];
	uint32_t num;
}

struct Name {
	// So with 100,000 lines of code and a three names per line, that
	// is 300,000 names.
	uint32_t id[300000];
	uint32_t start[300000];
	uint32_t end[300000];
	uint32_t num;
}

struct Argument {
	// Say there is an argument given twice on each line of code,
	// for 100,000 lines of code, that is 200,000 arguments in a module.
	uint32_t parent[200000];
	uint32_t child[200000];
	uint32_t num;
}

struct SwitchBranch {
	// Say there is a switch (case-of or if-else) every 20 lines of code
	// in 100,000 lines of code, and that the average switch has 4
	// branches, that is 20,000 branches.
	uint32_t on[20000];
	uint32_t pattern[20000];
	uint32_t result[20000];
	uint16_t num;
}

struct ListLiteral {
	// Say there is a list literal every 20 lines of code, that is
	// 5000 list literals in the module.
	uint32_t first_item[5000];
	uint32_t length[5000];
	uint16_t num;
}

struct Bind {
	// Say there is a wrapper every 3 lines of code, that comes to
	// 33,333.
	uint32_t bound[33333];
	uint16_t num;
}

struct Ast {
	struct Ordering ordering
	struct Name name;
	struct BlockComment block_comment;
	struct LineComment line_comment;
	struct Argument argument;
	struct SwitchBranch switch_branch;
	uint8_t export_all; // boolean
	struct ListLiteral list_literal;
	struct Bind bind;
}

int main(int argc, char* argv[]) {
	FILE* elm_dot_json = fopen("elm.json", "rb");
	if (elm_dot_json == NULL) {
		perror("could not find elm.json file\n");
		return -1;
	}

}
