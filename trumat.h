#include <stdint.h>
// I found an elm file in elm-review that was approx 700 KB.
#define CODE_SIZE 2000 * 1000

// This is all the memory that is used by the formatter except stack. It
// can be allocated when the program starts up and reused whenever the
// format function is used.
//
// The format function does no dynamic memory allocation.
//
// You should call zero_memory on it before passing it in.
struct memory {};

// Clears the memory used by the formatter. It doesn't necessarily overwrite
// it but it will set lengths and counters to zero.
void zero_memory(struct memory *);

// This is the formatter.
int format(
    // This should contain the unformatted source code terminated with a
    // zero byte.
    uint8_t in[CODE_SIZE],
    // This will be overwritten with the formatted code, terminated with a
    // zero byte.
    uint8_t out[CODE_SIZE], struct memory *m);
