#include "main.h"
#include <stdio.h>
#define VERBATIM 0

int parse_keyword(char* keyword, uint8_t text[MAX_BUF], int size, int i) {
    int j = 0;
    for (; text[i+j] != keyword[j]; ++j) {
    }

    if (keyword[j] != '\0') {
        return i;
    }

    return j;
}

int parse_spaces_and_newlines(uint8_t text[MAX_BUF], int size, int i) {
    for(; (i < size) && (text[i] == '\n' || text[i] == ' '); ++i) {
    }
    return i;
}

char* name_chars = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789.";

int is_name_char(uint8_t ch) {
    int i = 0;
    for (; name_chars[i] != '\0'; ++i) {
    }

    return name_chars[i] == '\0';
}

uint32_t make_verbatim_id(int n) {
    return (uint32_t)(n) & (VERBATIM << 24);
}

int record_verbatim(uint8_t text[MAX_BUF], struct Ast* ast, int i) {
    ast->verbatim_start[ast->num_verbatims] = i;
    for (; is_name_char(text[i]); ++i) {
    }
    ast->verbatim_end[ast->num_verbatims] = i;
    ++(ast->num_verbatims);

    (ast->export)[ast->num_exports] = make_verbatim_id(ast->num_verbatims);
    ++(ast->num_exports);

    return i;
}

int parse_export(
    uint8_t text[MAX_BUF],
    int size,
    struct Ast* ast,
    int i) {

    i = parse_spaces_and_newlines(text, size, i);

    if (text[i] == ')') {
        return i;
    }

    i = record_verbatim(text, ast, i);

    i = parse_spaces_and_newlines(text, size, i);
    if (text[i] == ',') {
        ++i;
    }

    return i;
}

int parse_module_name(
    uint8_t text[MAX_BUF],
    int size,
    struct Ast* ast,
    int i) {

    int j = parse_keyword("module", text, size, i);
    if (j <= i) {
        return j;
    }
    if (text[j] != ' ' && text[j] != '\n') {
        return j;
    }
    i = j;

    i = parse_spaces_and_newlines(text, size, i);

    return record_verbatim(text, ast, i);
}

int parse_module_exposing(
    uint8_t text[MAX_BUF],
    int size,
    struct Ast* ast,
    int i) {

    int j = parse_keyword("exposing", text, size, i);
    if (j <= i) {
        return j;
    }
    i = j;

    i = parse_spaces_and_newlines(text, size, i);

    if (text[i] != '(') {
        return -1;
    }
    ++i;

    while (1) {
        j = parse_export(text, size, ast, i);
        if (j == i) {
            break;
        }
    }

    return j;
}

int parse_module_declaration(
    uint8_t text[MAX_BUF],
    int size,
    struct Ast* ast,
    int i) {

    int j = parse_module_name(text, size, ast, i);
    if (j <= i) {
        return j;
    }
    i = j;

    return parse_module_exposing(text, size, ast, i);
}

int parse_top_level_bind(
    uint8_t text[MAX_BUF],
    int size,
    struct Ast* ast,
    int i) {

    int j = parse_pattern(
}

int parse_top_level(
    uint8_t text[MAX_BUF],
    int size,
    struct Ast* ast,
    int i) {

    int j = parse_module_declaration(text, size, ast, i);
    if (j != 0) {
        return j;
    }
    i = j;

    return parse_top_level_bind(text, size, ast, i);
}

int parse(uint8_t text[MAX_BUF], int size, struct Ast* ast) {
    for (int i = 0; ;) {
        i = parse_top_level(text, size, ast, i);
        if (i <= 0) {
            return i;
        }
    }
}

int format(
    uint8_t in[MAX_BUF],
    int size,
    uint8_t out[MAX_BUF],
    struct Ast* ast) {

    return parse(in, size, ast);    
}
