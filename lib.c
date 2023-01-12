#include "lib.h"

int format(struct Buf* buf, struct Ast* ast) {
	return 0;
}

void init_verbatim(struct Verbatim* verbatim) {
	verbatim->n = 0;
}

void init_ast(struct Ast* ast) {
	init_verbatim(&ast->verbatim);
}
