#include "text.h"

// This is the formatter.
int format(
    // This should contain the unformatted source code.
    struct text in,
    // This will be overwritten with the formatted code.
    struct text *out, struct text_memory *m);

// Writes "parent" and "child" to "result", separated by a forward slash.
void make_sub_path(const char *parent, const char *child, char *result);

// Checks if the path is "." or "..".
int is_dot_path(const char *);

// Checks if the path ends in .elm
int is_elm_path(const char *);
