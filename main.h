#include <stdint.h>

#define MAX_BUF 10*1000*1000
#define MAX_EXPORTS 1000
#define MAX_MODULE_MEMBERS 10000
#define MAX_BINDS 10000
#define MAX_LITERAL 100000
#define MAX_VERBATIM 100000

struct Mem {
    uint8_t delete_space[MAX_BUF];
    uint8_t delete_newline[MAX_BUF];
    uint8_t insert_space[MAX_BUF];
    uint8_t insert_newline[MAX_BUF];
};

int format(uint8_t in[MAX_BUF], int size, uint8_t out[MAX_BUF], struct Mem* mem);
