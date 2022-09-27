#include <stdio.h>
#include <stdint.h>

enum ErrorCode {
	NoElmDotJson,
};

// Assumes that there are:
//  + 1,000,000 lines of code
//  + 50 characters per line
//  + an average of 500 lines per module
//  + so there are 2000 modules
//  + so there are 50,000,000 characters
struct Db {
    // Assume there are not more than 100 source directories.
    // Assume that the average directory name size is 10
    // characters, so there are 1000 characters.
    //
    // Directory names are ended with 3, end of text, and any
    // empty space at the end is filled with 0, null.
    uint8_t src_dirs[1000];

    // Assume there are not more than 1000 direct dependencies.
    // Using the same logic as above.
    uint8_t direct_deps[10000];

    // The modules are ended by 28, the file separator, as
    // long as it isn't inside a string literal.
    //
    // Any empty space at the end is filled with 0, null.
    uint8_t src[50000000];
}

int main(int argc, char* argv[]) {
	FILE* elm_dot_json = fopen("elm.json", "rb");
	if (elm_dot_json == NULL) {
		perror("could not find elm.json file\n");
		return -1;
	}

}
