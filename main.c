#include "main.h"
#include <stdio.h>
#define VERBATIM_EXPRESSION 0

char* END_NAME_CHARS = " \n-+=(){}[]/*&!";

int is_end_name_char(uint8_t ch) {
    for (int i = 0; END_NAME_CHARS[i] != '\0'; ++i) {
        if (ch == END_NAME_CHARS[i]) {
            return 1;
        }
    }

    return 0;
}

int is_name_char(uint8_t ch) {
    return
        (ch > 'a' && ch < 'z')
            || (ch > 'A' && ch < 'Z')
            || (ch > '0' && ch < '9')
            || ch == '_'
            || ch == '.';
}

int is_first_name_char(uint8_t ch) {
    return
        (ch > 'a' && ch < 'z')
            || (ch > 'A' && ch < 'Z')
            || ch == '_';
}

int parse_name(
    uint8_t in[MAX_BUF],
    int size,
    struct Ast* ast,
    int i) {

    if (i < size && !is_first_name_char(in[i])) {
        return i;
    }

    int j = i;
    for (; j < size && is_name_char(in[j]); ++j) {
    }

    if (j < size && !is_end_name_char(in[j])) {
        return i;
    }

    ast->verbatim_start[ast->num_verbatims] = i;
    ast->verbatim_end[ast->num_verbatims] = j;
    ++(ast->num_verbatims);

    return j;
}

int parse_keyword(
    char* keyword,
    uint8_t in[MAX_BUF],
    int i,
    int size) {

    int j = i;
    for (; j < size && keyword[j-i] != '\0'; ++j) {
        if (in[j] != keyword[j-i]) {
            return i;
        }
    }

    return j;
}

int consume_whitespace(
    uint8_t in[MAX_BUF],
    int size,
    int i) {

    for (; i < size && (in[i] == ' ' || in[i] == '\n'); ++i) {
    }

    return i;
}

int consume_exports_spaces(
    uint8_t in[MAX_BUF],
    int size,
    struct Ast* ast,
    int i) {
    
    for (; i < size && (in[i] == ' ' || in[i] == '\n'); ++i) {
        if (in[i] == '\n') {
            ast->newline_exports = 1;
        }
    }
    return i;
}

int parse_export(
    uint8_t in[MAX_BUF],
    int size,
    struct Ast* ast,
    int i) {

    int j = parse_name(in, size, ast, i);
    if (j <= i) {
        return -1;
    }
    i = j;

    ast->export[ast->num_exports] = ast->num_verbatims - 1;
    ++(ast->num_exports);

    if (i < size && in[i] != '(') {
        return i;
    }

    for (; i < size && in[i] != ')'; ++i) {
    }
    ++i;

    ast->export_expose[ast->num_export_expose] = ast->num_exports - 1;
    ++(ast->num_export_expose);

    return i;
}

int parse_module_declaration(
    uint8_t in[MAX_BUF],
    int size,
    struct Ast* ast,
    int i) {

    int j = parse_keyword("module", in, i, size);
    if (j <= i) {
        return j;
    }
    i = j;

    j = parse_name(in, size, ast, i);
    if (j <= i) {
        return j;
    }
    i = j;

    ast->has_module_name = 1;
    ast->module_name = ast->num_verbatims - 1;

    i = consume_whitespace(in, size, i);

    j = parse_keyword("exposing", in, i, size);
    if (j <= i) {
        return -1;
    }

    i = consume_whitespace(in, size, i);

    if (!(i < size && in[i] == '(')) {
        return -1;
    }
    ++i;

    for (; i < size && in[i] != ')';) {
        i = consume_exports_spaces(in, size, ast, i);
        i = parse_export(in, size, ast, i);
        if (i < 0) {
            return i;
        }
        i = consume_exports_spaces(in, size, ast, i);
        if (i < size && in[i] == ')') {
            ++i;
            return i;
        }
        if (i < size && in[i] == ',') {
            ++i;
            continue;
        }
        return -1;
    }
    ++i;

    return i;
}

int is_digit(uint8_t ch) {
    return ch >= '0' && ch <= '9';
}

int is_end_int(uint8_t ch) {
    return ch == ' ' || ch == '\n' || ch == ')' || ch == ']' || ch == '}';
}

int parse_int(
    uint8_t in[MAX_BUF],
    int size,
    struct Ast* ast,
    int i) {

    int j = i;
    for (; j < size && is_digit(in[j]); ++j) {
    }

    if (!(j < size && is_end_int(in[j]))) {
        return -1;
    }

    ast->verbatim_start[ast->num_verbatims] = i;
    ast->verbatim_end[ast->num_verbatims] = j;
    ++(ast->num_verbatims);

    ast->expression_type[ast->num_expressions] = VERBATIM_EXPRESSION;
    ast->expression_id[ast->num_expressions] = ast->num_verbatims - 1;
    ++(ast->num_expressions);

    return j;
}

int parse_expression(
    uint8_t in[MAX_BUF],
    int size,
    struct Ast* ast,
    int i) {

    return parse_int(in, size, ast, i);
}

int parse_top_level_bind(
    uint8_t in[MAX_BUF],
    int size,
    struct Ast* ast,
    int i) {

    int j = parse_name(in, size, ast, i);
    if (j <= i) {
        return i;
    }
    i = j;

    i = consume_whitespace(in, size, i);

    if (!(i < size && in[i] == '=')) {
        return -1;
    }

    i = parse_expression(in, size, ast, i);
    if (i < 0) {
        return i;
    }

    ast->bind_left[ast->num_binds] = ast->num_verbatims - 1;
    ast->bind_right[ast->num_expressions] = ast->num_expressions - 1;
    ++(ast->num_binds);

    ast->is_top_bind[ast->num_is_top_binds - 1] = ast->num_binds - 1;
    ++(ast->num_is_top_binds);

    return i;
}

int parse_top_level(
    uint8_t in[MAX_BUF],
    int size,
    struct Ast* ast,
    int i) {

    int j = parse_module_declaration(in, size, ast, i);
    if (j != i) {
        return j;    
    }
    i = j;

    return parse_top_level_bind(in, size, ast, i);
}

int parse(
    uint8_t in[MAX_BUF],
    int size,
    struct Ast* ast) {

    for (int i = 0;;) {
        int j = parse_top_level(in, size, ast, i);
        if (j <= i) {
            return j;
        }
        i = j;
        i = consume_whitespace(in, size, i);
    }
}

int format(
    uint8_t in[MAX_BUF],
    int size,
    uint8_t out[MAX_BUF],
    struct Ast* ast) {

    return parse(in, size, ast);    
}
