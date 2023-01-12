#include <stdint.h>
#define BUF_MAX 10*1000*1000
#define VERBATIM_ITEMS 100*1000
#define VERBATIM_MEAN 20
#define VERBATIM_CHARS (VERBATIM_ITEMS * VERBATIM_MEAN)

struct Verbatim {
	uint8_t chars[VERBATIM_CHARS];
	uint32_t ends[VERBATIM_ITEMS];
	int n;
};

struct Ast {
	struct Verbatim verbatim;
};

struct Buf {
	uint8_t chars[BUF_MAX];
	int size;
};

void init_ast(struct Ast*);

void init_buf(struct Buf*);

int format(struct Buf*, struct Ast*);
