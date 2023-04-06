#include "main.h"

int format_module_header(FILE* in, FILE* out) {
    return 0;
}

int format(FILE* in, FILE* out) {
    int result = format_module_header(in, out);
    if (result != 0) {
        return result;
    }
}
