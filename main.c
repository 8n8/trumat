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
        return ExportName;
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

    i = consume_whitespace(in, size, i);

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
        return Exposing;
    }
    i = j;

    i = consume_whitespace(in, size, i);

    if (!(i < size && in[i] == '(')) {
        return OpenBracketAfterExposing;
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
        return ExposingList;
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
        return i;
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

    ast->bind_left[ast->num_binds] = ast->num_verbatims - 1;

    i = consume_whitespace(in, size, i);

    if (!(i < size && in[i] == '=')) {
        return EqualsInTopLevelBind;
    }
    ++i;

    i = consume_whitespace(in, size, i);

    i = parse_expression(in, size, ast, i);
    if (i < 0) {
        return i;
    }

    ast->bind_right[ast->num_binds] = ast->num_expressions - 1;
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

int print_string(char* string, int i, uint8_t out[MAX_BUF]) {
    int j = 0;
    for (; string[j] != '\0'; ++j) {
        out[i+j] = string[j];
    }
    return i + j; 
}

int print_verbatim(
    struct Ast* ast,
    uint8_t in[MAX_BUF],
    uint8_t out[MAX_BUF],
    int i,
    uint32_t verbatim) {

    uint32_t start = ast->verbatim_start[verbatim];
    uint32_t end = ast->verbatim_end[verbatim];

    int j = 0;
    for (; j < (end - start); ++j) {
        out[i+j] = in[start+j];
    }

    return i+j;
}

int print_first_oneline_export(
    struct Ast* ast,
    uint8_t in[MAX_BUF],
    uint8_t out[MAX_BUF],
    int export_id,
    int i) {

    i = print_string("(", i, out);
    return print_verbatim(ast, in, out, i, ast->export[export_id]);
}

int print_first_newline_export(
    struct Ast* ast,
    uint8_t in[MAX_BUF],
    uint8_t out[MAX_BUF],
    int export_id,
    int i) {

    i = print_string("\n    ( ", i, out);
    return print_verbatim(ast, in, out, i, ast->export[export_id]);
}

int print_subsequent_oneline_export(
    struct Ast* ast,
    uint8_t in[MAX_BUF],
    uint8_t out[MAX_BUF],
    int export_id,
    int i) {

    i = print_string(", ", i, out);
    return print_verbatim(ast, in, out, i, ast->export[export_id]);
}

int print_oneline_exports(
    struct Ast* ast,
    uint8_t in[MAX_BUF],
    uint8_t out[MAX_BUF],
    int i) {

    i = print_first_oneline_export(ast, in, out, 0, i);
    for (int j = 1; j < ast->num_exports; ++j) {
        i = print_subsequent_oneline_export(ast, in, out, j, i);
    }
    return print_string(")", i, out);
}

int print_newline_exports(
    struct Ast* ast,
    uint8_t in[MAX_BUF],
    uint8_t out[MAX_BUF],
    int i) {

    i = print_first_newline_export(ast, in, out, 0, i);
    for (int j = 1; j < ast->num_exports; ++j) {
        i = print_subsequent_oneline_export(ast, in, out, j, i);
    }
    return print_string("\n    )", i, out);
}

int print_exports(
    struct Ast* ast,
    uint8_t in[MAX_BUF],
    uint8_t out[MAX_BUF],
    int i) {

    if (ast->newline_exports) {
        return print_newline_exports(ast, in, out, i);
    }

    return print_oneline_exports(ast, in, out, i);
}

int print_expression(
    struct Ast* ast,
    uint8_t in[MAX_BUF],
    uint8_t out[MAX_BUF],
    int i,
    uint8_t expression_id) {

    enum Expression expression_type = ast->expression_type[expression_id];
    switch (expression_type) {
    case Verbatim:
        uint8_t verbatim_id = ast->expression_id[expression_id];
        return print_verbatim(ast, in, out, i, verbatim_id);
    }

    return InvalidExpression;
}

int print_top_level_bind(
    struct Ast* ast,
    uint8_t in[MAX_BUF],
    uint8_t out[MAX_BUF],
    int i,
    uint8_t bind_id) {

    uint32_t left = ast->bind_left[bind_id];
    uint32_t right = ast->bind_right[bind_id];

    i = print_verbatim(ast, in, out, i, left);
    i = print_string(" =\n    ", i, out);
    return print_expression(ast, in, out, i, right);
}

int print_top_level_binds(
    struct Ast* ast,
    uint8_t in[MAX_BUF],
    uint8_t out[MAX_BUF],
    int i) {

    int j = 0;
    for (; j < ast->num_is_top_binds - 1; ++j) {
        i = print_top_level_bind(ast, in, out, i, j);
        i = print_string("\n\n\n", i, out);
    }

    i = print_top_level_bind(ast, in, out, i, j);
    i = print_string("\n", i, out);

    return i;
}

int print(struct Ast* ast, uint8_t in[MAX_BUF], uint8_t out[MAX_BUF]) {
    int i = print_string("module ", 0, out);
    i = print_verbatim(ast, in, out, i, ast->module_name);
    i = print_string(" exposing ", i, out);
    i = print_exports(ast, in, out, i);
    i = print_string("\n\n\n", i, out);
    return print_top_level_binds(ast, in, out, i);
}

int format(
    uint8_t in[MAX_BUF],
    int size,
    uint8_t out[MAX_BUF],
    struct Ast* ast) {

    int i = parse(in, size, ast);
    if (i != size) {
        return EndOfInput;
    }

    return print(ast, in, out);
}
