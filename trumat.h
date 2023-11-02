#include <stdint.h>
// I found an elm file in elm-review that was approx 700 KB.
#define CODE_SIZE 2000 * 1000
// This is all the text that is created while formatting one file.
#define TEXT_SIZE 20 * 1000 * 1000

// This is all the memory that is used by the formatter except stack. It
// can be allocated when the program starts up and reused whenever the
// format function is used.
//
// The format function does no dynamic memory allocation.
//
// You should call zero_memory on it before passing it in.
struct memory {
  uint8_t text[TEXT_SIZE];
  int text_head;
};

// Clears the memory used by the formatter. It doesn't necessarily overwrite
// it but it will set lengths and counters to zero.
void zero_memory(struct memory *);

// This is the formatter.
int format(
    // This should contain the unformatted source code terminated with a
    // zero byte.
    const uint8_t in[CODE_SIZE],
    // This will be overwritten with the formatted code, terminated with a
    // zero byte.
    uint8_t out[CODE_SIZE], struct memory *m);

// Writes "parent" and "child" to "result", separated by a forward slash.
void make_sub_path(const char *parent, const char *child, char *result);

// Checks if the path is "." or "..".
int is_dot_path(const char *);

// Checks if the path ends in .elm
int is_elm_path(const char *);
