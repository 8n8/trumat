#include "main.h"

char* parse_literal(char* in, char* literal) {
    char* i = in;
    for (; *i == *literal && *literal != '\0'; ++i && ++literal) {
    }

    if (*i == '\0') {
        return i;
    }

    return in;
}

char* copy_literal(char* out, char* literal) {
    for (; *literal != '\0'; ++out && ++literal) {
        *out = *literal;
    }
    return out;
}

int format_module_header(char** in, char** out) {
    int result = parse_literal(*in, "module ");
    if (result == in) {
        return in;
    }
    out = copy_literal(out, "module ");
    in = result;
}

int format(char* in, char* out) {
    int result = format_module_header(&in, &out);
    if (result < 0) {
        return result;
    }
}
