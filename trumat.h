#include "text.h"

// This is all the memory that is used by the formatter except stack. It
// can be allocated when the program starts up and reused whenever the
// format function is used.
//
// The format function does no dynamic memory allocation.
//
// You should call zero_memory on it before passing it in.
struct memory {
  struct text_memory text;
};

// Clears the memory used by the formatter. It doesn't necessarily overwrite
// it but it will set lengths and counters to zero.
void zero_memory(struct memory *);

// This is the formatter.
int format(
    // This should contain the unformatted source code.
    struct text in,
    // This will be overwritten with the formatted code.
    struct text *out, struct memory *m);

// Writes "parent" and "child" to "result", separated by a forward slash.
void make_sub_path(const char *parent, const char *child, char *result);

// Checks if the path is "." or "..".
int is_dot_path(const char *);

// Checks if the path ends in .elm
int is_elm_path(const char *);
