#include "main.h"

#define EOF_WHILE_WRITING_CHUNK -1
#define EOF_WHILE_PARSING_CHUNK -2
#define EOF_IN_FIRST_NAME_CHAR -3
#define NOT_FIRST_NAME_CHAR -4
#define EOF_WHEN_WRITING_FIRST_NAME_CHAR -5
#define NOT_SUBSEQUENT_NAME_CHAR -6
#define EOF_WHEN_WRITING_SUBSEQUENT_NAME_CHAR -7
#define COULDNT_PARSE_NAME -8

int write_chunk(FILE* file, char* chunk) {
    for (; *chunk != '\0'; ++chunk) {
        if (fputc(*chunk, file) == EOF) {
            return EOF_WHILE_WRITING_CHUNK;
        }
    }

    return 0;
}

int parse_chunk(FILE* file, char* chunk) {
    int initial_offset = ftell(file);

    for (; *chunk != '\0'; ++chunk) {
        int result = fgetc(file);
        if (result == EOF) {
            return EOF_WHILE_PARSING_CHUNK;
        }

        if (result != *chunk) {
            return fseek(file, initial_offset, SEEK_SET);
        }
    }

    return 0;
}

int is_subsequent_name_char(char ch) {
    return
        (ch >= 'a' && ch <= 'z') ||
        (ch >= 'A' && ch <= 'Z') ||
        (ch >= '0' && ch <= '9') ||
        (ch == '.') ||
        (ch == '_');
}

int is_first_name_char(char ch) {
    return
        (ch >= 'a' && ch <= 'z') ||
        (ch >= 'A' && ch <= 'Z') ||
        (ch == '.') ||
        (ch == '_');
}

int is_after_name_char(char ch) {
    return
        ch == ' ' ||
        ch == '(' ||
        ch == ')' ||
        ch == '*' ||
        ch == '+' ||
        ch == '-' ||
        ch == '/' ||
        ch == ':' ||
        ch == '<' ||
        ch == '=' ||
        ch == '>' ||
        ch == '[' ||
        ch == ']' ||
        ch == '{' ||
        ch == '|' ||
        ch == '}';
}

int move_back_one_char(FILE* file) {
    int result = ftell(file);
    if (result < 0) {
        return result;
    }
    return fseek(file, result - 1, SEEK_SET);
}

int parse_name(FILE* in, FILE* out) {
    int initial_in_offset = ftell(in);
    int initial_out_offset = ftell(out);

    int in_char = fgetc(in);
    if (in_char == EOF) {
        return EOF_IN_FIRST_NAME_CHAR;
    }

    if (!is_first_name_char(in_char)) {
        fseek(in, initial_in_offset, SEEK_SET);
        return NOT_FIRST_NAME_CHAR;
    }

    if (fputc(in_char, out) == EOF) {
        return EOF_WHEN_WRITING_FIRST_NAME_CHAR;
    }

    in_char = fgetc(in);

    for (; in_char != EOF; ) {
        if (is_after_name_char(in_char)) {
            return move_back_one_char(in);
        }

        if (!is_subsequent_name_char(in_char)) {
            fseek(in, initial_in_offset, SEEK_SET);
            fseek(out, initial_out_offset, SEEK_SET);
            return NOT_SUBSEQUENT_NAME_CHAR;
        }

        if (fputc(in_char, out) == EOF) {
            return EOF_WHEN_WRITING_SUBSEQUENT_NAME_CHAR;
        }

        in_char = fgetc(in);
    }

    return COULDNT_PARSE_NAME;
}

int consume_all_whitespace(FILE* file) {
    for (int result = fgetc(file); result == ' ' || result == '\n'; ) {
        result = fgetc(file);
    }
    return move_back_one_char(file);
}

int format_module_header(FILE* in, FILE* out) {
    int result = parse_chunk(in, "module ");
    if (result) {
        return result;
    }

    result = write_chunk(out, "module ");
    if (result) {
        return result;
    }

    result = parse_name(in, out);
    if (result) {
        return result;
    }

    result = consume_all_whitespace(in);
    if (result) {
        return result;
    }

    return parse_chunk(in, "exposing");
}

int format(FILE* in, FILE* out) {
    int result = format_module_header(in, out);
    if (result != 0) {
        return result;
    }

    return 0;
}
