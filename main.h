#include <stdint.h>

#define MAX_BUF 10*1000*1000
#define MAX_EXPORTS 1000
#define MAX_MODULE_MEMBERS 10000
#define MAX_BINDS 10000
#define MAX_LITERAL 100000
#define MAX_VERBATIM 100000
#define MAX_EXPRESSIONS 100000

struct Ast {
    uint32_t verbatim_start[MAX_VERBATIM];
    uint32_t verbatim_end[MAX_VERBATIM];
    int num_verbatims;

    int has_module_name;
    uint32_t module_name;

    uint32_t export[MAX_EXPORTS];
    int num_exports;
    uint32_t export_expose[MAX_EXPORTS];
    int num_export_expose;
    int newline_exports;

    uint32_t bind_left[MAX_BINDS];
    uint32_t bind_right[MAX_BINDS];
    int num_binds;

    uint32_t is_top_bind[MAX_BINDS];
    int num_is_top_binds;

    uint32_t list_id[MAX_EXPRESSIONS];
    uint32_t list_item[MAX_EXPRESSIONS];
    int num_list_items;

    uint32_t has_newlines[MAX_EXPRESSIONS];
    int num_has_newlines;

    uint8_t expression_type[MAX_EXPRESSIONS];
    uint32_t expression_id[MAX_EXPRESSIONS];
    int num_expressions;

    uint32_t next_free_id;
};

int format(uint8_t in[MAX_BUF], int size, uint8_t out[MAX_BUF], struct Ast* ast);

enum Error {
    ExportName = -1,
    Exposing = -2,
    OpenBracketAfterExposing = -3,
    ExposingList = -4,
    EqualsInTopLevelBind = -5,
    EndOfInput = -6,
    InvalidExpression = -7,
    ListError = -8,
    ListItemError = -9,
};

enum Expression {
    Verbatim = 0,
    NonEmptyList = 1,
    EmptyList = 2,
};
