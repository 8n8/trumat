#include "main.h"

#define UNEXPECTED_EOF -1
#define NOT_FIRST_NAME_CHAR -2
#define NOT_SUBSEQUENT_NAME_CHAR -3
#define COULDNT_PARSE_NAME -4
#define CHAR_NOT_FOUND -5
#define COULDNT_PARSE_CHAR -6

int write_chunk(FILE* file, char* chunk) {
    for (; *chunk != '\0'; ++chunk) {
        if (fputc(*chunk, file) == EOF) {
            return UNEXPECTED_EOF;
        }
    }

    return 0;
}

int write_char(FILE* file, char ch) {
    int result = fputc(ch, file);
    if (result == EOF) {
        return UNEXPECTED_EOF;
    }

    return 0;
}

int parse_chunk(FILE* file, char* chunk) {
    int initial_offset = ftell(file);

    for (; *chunk != '\0'; ++chunk) {
        int result = fgetc(file);
        if (result == EOF) {
            return UNEXPECTED_EOF;
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
        return UNEXPECTED_EOF;
    }

    if (!is_first_name_char(in_char)) {
        fseek(in, initial_in_offset, SEEK_SET);
        return NOT_FIRST_NAME_CHAR;
    }

    if (fputc(in_char, out) == EOF) {
        return UNEXPECTED_EOF;
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
            return UNEXPECTED_EOF;
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

int consume_spaces(FILE* file) {
    for (int result = fgetc(file); result == ' '; ) {
        result = fgetc(file);
    }
    return move_back_one_char(file);
}

int parse_char(FILE* file, char ch) {
    int result = fgetc(file);
    if (result == EOF) {
        return UNEXPECTED_EOF;
    }

    if (result == ch) {
        return 0;
    }

    result = move_back_one_char(file);
    if (result) {
        return result;
    }

    return COULDNT_PARSE_CHAR;
}

int parse_export(FILE* in, FILE* out) {
    int result = parse_name(in, out);
    if (result) {
        return result;
    }

    result = parse_char(in, '(');
    if (result) {
        return 0;
    }

    int nesting = 1;
    while (nesting > 0) {
        result = fgetc(in);
        if (result == EOF) {
            return UNEXPECTED_EOF;
        }

        if (result == '(') {
            ++nesting;
        }

        if (result == ')') {
            --nesting;
        }
    }

    return write_chunk(out, "(..)");
}

int format_newline_export_list(FILE* in, FILE* out) {
    int result = write_chunk(out, "\n    (");
    if (result) {
        return result;
    }

    while (1) {
        int result = consume_spaces(in);
        if (result) {
            return result;
        }

        result = parse_export(in, out);
        if (result) {
            result = parse_char(in, ')');
            if (result) {
                return result;
            }
            break;
        }

        result = consume_spaces(in);
        if (result) {
            return result;
        }

        result = parse_char(in, ')');
        if (!result) {
            break;
        }

        result = parse_char(in, ',');
        if (result) {
            return result;
        }

        result = write_chunk(out, "\n    , ");
        if (result) {
            return result;
        }
    }

    return write_chunk(out, "\n    )");
}

int format_oneline_export_list(FILE* in, FILE* out) {
    int result = write_chunk(out, " ("); 
    if (result) {
        return result;
    }
    while (1) {
        int result = consume_spaces(in);
        if (result) {
            return result;
        }

        result = parse_export(in, out);
        if (result) {
            result = parse_char(in, ')');
            if (result) {
                return result;
            }
            break;
        }

        result = consume_spaces(in);
        if (result) {
            return result;
        }

        result = parse_char(in, ')');
        if (!result) {
            break;
        }

        result = parse_char(in, ',');
        if (result) {
            return result;
        }

        result = write_chunk(out, ", ");
        if (result) {
            return result;
        }
    }

    return write_char(out, ')');
}

int get_exports_have_newlines(FILE* in) {
    int initial_offset = ftell(in);

    int nesting = 1;
    while (nesting > 0) {
        int result = fgetc(in);
        if (result == EOF) {
            return UNEXPECTED_EOF;
        }

        if (result == '\n') {
            printf("found newline in exports\n");
            fseek(in, initial_offset, SEEK_SET);
            return 1;
        }

        if (result == '(') {
            ++nesting;
        }

        if (result == ')') {
            --nesting;
        }
    }

    fseek(in, initial_offset, SEEK_SET);
    return 0;
}

int format_export_list(FILE* in, FILE* out) {
    int result = parse_char(in, '(');
    if (result) {
        return result;
    }

    int has_newlines = get_exports_have_newlines(in);
    if (has_newlines < 0) {
        return has_newlines;
    }

    if (has_newlines) {
        return format_newline_export_list(in, out);
    }

    return format_oneline_export_list(in, out);
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

    result = parse_chunk(in, "exposing");
    if (result) {
        return result;
    }

    result = write_chunk(out, " exposing");
    if (result) {
        return result;
    }

    result = consume_all_whitespace(in);
    if (result) {
        return result;
    }

    return format_export_list(in, out);
}

int format_int_literal(FILE* in, FILE* out) {
    while(1) {
        int result = fgetc(in);
        if (result == EOF) {
            return UNEXPECTED_EOF;
        }

        if (!(result >= '0' && result <= '9')) {
            break;
        }
    }

    return 0;
}

int format_expression(FILE* in, FILE* out, int indent) {
    return format_int_literal(in, out);
}

int format_top_level_bind(FILE* in, FILE* out) {
    int result = parse_name(in, out);
    if (result) {
        return result;
    }

    result = consume_spaces(in);
    if (result) {
        return result;
    }

    result = parse_char(in, '=');
    if (result) {
        return result;
    }

    result = consume_all_whitespace(in);
    if (result) {
        return result;
    }

    result = write_chunk(out, " =\n    ");
    if (result) {
        return result;
    }

    return format_expression(in, out, 4);
}

int format_top_level_binds(FILE* in, FILE* out) {
    while (1) {
        int result = consume_all_whitespace(in);
        if (result) {
            return result;
        }

        result = format_top_level_bind(in, out);
        if (result) {
            return 0;
        }
    }
}

int format(FILE* in, FILE* out) {
    int result = format_module_header(in, out);
    if (result) {
        return result;
    }

    result = write_chunk(out, "\n\n\n");
    if (result) {
        return result;
    }

    return format_top_level_binds(in, out);
}
