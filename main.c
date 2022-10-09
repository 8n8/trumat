#include <stdio.h>
#include <stdint.h>
#include <dirent.h>
#include <errno.h>

#define MAX_PATH 4096

void make_file_path(
	char file_path[MAX_PATH],
	char directory_path[MAX_PATH],
	char file_name[256]) {

	// copy the directory path
	int i = 0;
	for (; i < MAX_PATH; ++i) {
		if (directory_path[i] == '\0') {
			break;
		}
		file_path[i] = directory_path[i];
	}

	file_path[i] = '/';
	++i;
	
	// copy the file name
	for (int j = 0; j < 256; ++j) {
		if (file_name[j] == '\0') {
			file_path[i+j] = '\0';
			return;
		}

		file_path[i + j] = file_name[j];
	}
	printf("SHOULDNT HAPPEN");
}

void make_sub_dir_path(
	char sub_dir_path[MAX_PATH],
	char directory_path[MAX_PATH],
	char sub_dir_name[256]) {

	int i = 0;
	for (; i < MAX_PATH; ++i) {
		if (directory_path[i] == '\0') {
			break;
		}

		sub_dir_path[i] = directory_path[i];
	}

	sub_dir_path[i] = '/';
	++i;

	for (int j = 0; j < 256; ++j) {
		if (sub_dir_name[j] == '\0') {
			sub_dir_path[i+j] = '\0';
			return;
		}
		sub_dir_path[i + j] = sub_dir_name[j];
	}
}

int is_relevant_dir(char dir_name[256]) {
	if (dir_name[0] == '.' && dir_name[1] == '\0') {
		return 0;
	}

	if (
		dir_name[0] == '.' &&
		dir_name[1] == '.' &&
		dir_name[2] == '\0') {

		return 0;
	}

	return 1;
}

int is_elm_file(char file_name[256]) {
	int size = 0;
	for (; size < 256; ++size) {
		if (file_name[size] == '\0') {
			break;
		}
	}
	
	// smallest Elm file name
	// 12345 size
	// 01234 index
	// X.elm
	return
		size > 4 &&
		file_name[size - 1] == 'm' &&
		file_name[size - 2] == 'l' &&
		file_name[size - 3] == 'e' &&
		file_name[size - 4] == '.';
}

#define CODE_BUF_SIZE 5000000

struct CodeBuffers {
	char one[CODE_BUF_SIZE];
	char two[CODE_BUF_SIZE];
	int two_size;
	int one_size;
};

struct CodeBuffers code_buffers;

int is_beginning_comma_separated(int i, char buf[CODE_BUF_SIZE], int size) {
	return buf[i] == '[' || buf[i] == '(' || buf[i] == '{';
}

int is_ending_special(int i, char buf[CODE_BUF_SIZE], int size) {
	return buf[i] == ']' || buf[i] == ')' || buf[i] == '}';
}

int is_beginning_verbatim_string(
	int i,
	char buf[CODE_BUF_SIZE],
	int size) {

	return
		size - i > 3 &&
		buf[i] == '"' &&
		buf[i+1] == '"' &&
		buf[i+2] == '"';
}

int is_ending_verbatim_string(
	int i,
	char buf[CODE_BUF_SIZE],
	int size) {

	return
		i > 3 &&
		buf[i-1] == '"' &&
		buf[i-2] == '"' &&
		buf[i-3] == '"';
}

int calculate_indent(
	int this_line_spaces,
	int previous_spaces,
	int previous_indent,
	int expression_spaces,
	int inside_special) {

	// x =
	//     [ "a"
	//     , [ "b"
	//       , "c"
	//       ]
	//     ]
	if (inside_special) {
		return
			expression_spaces +
			this_line_spaces -
			previous_spaces;
	}
}


void top_level_indent() {

	// the column
	int column = 0;

	// the number of spaces at the start of the current line
	int this_line_spaces = 0;

	// the number of spaces at the start of the previous line
	int last_line_spaces = 0;

	// the column of the start of the thing we are currently in, -1 if
	// not in one
	int sub_start_indent = 0;

	// whether or not we are parsing an indentation
	int parsing_indent = 1;

	int two_i = 0;
	for (int one_i = 0; one_i < code_buffers.one_size; ++one_i) {
		if (
			sub_start_indent < 0 &&
			is_beginning_sub(
				one_i,
				code_buffers.one,
				code_buffers.one_size)) {

			sub_start_indent = column;
		}

		if (
			sub_start_indent >= 0 &&
			is_ending_sub(
				one_i,
				code_buffers.one,
				code_buffers.one_size)) {

			sub_start_indent = -1;
		}

		if (code_buffers.one[one_i] == ' ' && parsing_indent) {
			++this_line_spaces;
		}

		if (code_buffers.one[one_i] == '\n') {
			column = 0;
			last_line_spaces = this_line_spaces;
			parsing_indent = 1;
		}

		if (code_buffers.one[one_i] != ' ' && parsing_indent) {
			parsing_indent = 0;

			int indent = calculate_indent(
				this_line_spaces,
				last_lines_spaces,

				this_line_indent_spaces,
				last_line_indent_spaces,
				last_line_indent,
				expression_indent,
				inside_special);

			for (int i = 0; i < indent; ++i) {
				code_buffers.two[two_i + i] = ' ';
			}

			two_i += indent;
			if (!inside_expression) {
				last_line_indent = indent;
			}
		}

		if (!parsing_indent) {
			code_buffers.two[two_i] = code_buffers.one[one_i];
			two_i++;
		}

		if (code_buffers.one[one_i] != '\n') {
			++column;
		}
	}
}

void no_trailing_space() {
	int num_consecutive_space = 0;

	int two_i = 0;
	for (int one_i = 0; one_i < code_buffers.one_size; ++one_i) {
		if (code_buffers.one[one_i] == ' ') {
			num_consecutive_space++;
		}

		if (
			code_buffers.one[one_i] != ' ' &&
			code_buffers.one[one_i] != '\n') {

			num_consecutive_space = 0;
		}

		if (
			code_buffers.one[one_i] == '\n' &&
			num_consecutive_space > 0) {

			two_i = two_i - num_consecutive_space;
			num_consecutive_space = 0;
		}

		code_buffers.two[two_i] = code_buffers.one[one_i];
		two_i++;
	}

	code_buffers.two_size = two_i;
}

int format_file(char path[MAX_PATH]) {
	printf("%s\n", path);

	FILE* handle_in = fopen(path, "rb");
	if (handle_in == NULL) {
		printf("failed to open the file: %s", path);
		return -1;
	}

	size_t n = fread(code_buffers.one, 1, CODE_BUF_SIZE, handle_in);
	if (!feof(handle_in)) {
		printf(
			"file too large: %s, max size is %d",
			path,
			CODE_BUF_SIZE);
		fclose(handle_in);
		return -1;
	}
	if (ferror(handle_in)) {
		printf("couldn't open input file: %s", path);
		fclose(handle_in);
		return -1;
	}

	code_buffers.one_size = n;

	// Add formatters in here.
	no_trailing_space();

	FILE* handle_out = fopen(path, "w");
	if (handle_out == NULL) {
		printf("couldn't open output file: %s", path);
		return -1;
	}

	n = fwrite(code_buffers.two, 1, code_buffers.two_size, handle_out);
	if (n != code_buffers.two_size) {
		printf("failed writing output to %s", path);
		fclose(handle_out);
		return -1;
	}
	fclose(handle_out);

	return 0;
}

int get_paths_from_dir(DIR*, char[MAX_PATH]);

int get_paths_from_fs_entry(DIR* d, char directory_path[MAX_PATH]) {
	struct dirent* dir = readdir(d);
	if (dir == NULL && errno != 0) {
		printf("couldn't read directory: %s", directory_path);
		return -1;
	}
	if (dir == NULL && errno == 0) {
		return 1;
	}
	if (dir->d_type == DT_REG && !is_elm_file(dir->d_name)) {
		return 0;
	}
	if (dir->d_type == DT_REG) {
		char file_path[MAX_PATH];
		make_file_path(file_path, directory_path, dir->d_name);

		int format_error = format_file(file_path);
		if (format_error != 0) {
			return format_error;
		}
	}
	if (dir->d_type == DT_DIR && !is_relevant_dir(dir->d_name)) {
		return 0;
	}
	if (dir->d_type != DT_DIR) {
		return 0;
	}
	char sub_dir_path[MAX_PATH];
	make_sub_dir_path(sub_dir_path, directory_path, dir->d_name);

	DIR* subd = opendir(sub_dir_path);
	if (subd == NULL) {
		printf("directory doesn't exist: \"%s\"\n", sub_dir_path);
		return -1;
	}
	int result = get_paths_from_dir(subd, sub_dir_path);
	closedir(subd);
	if (result != 0) {
		return result;
	}
	return 0;
}


int get_paths_from_dir(DIR* d, char directory_path[MAX_PATH]) {
	errno = 0;
	int result = 0;
	while (result == 0) {
		result = get_paths_from_fs_entry(d, directory_path);
	}
	if (result > 0) {
		return 0;
	}

	return result;
}

int main(int argc, char* argv[]) {
	DIR* d;
	char top_path[MAX_PATH];
	top_path[0] = '.';
	top_path[1] = '\0';
	d = opendir(top_path);
	if (d == NULL) {
	printf("directory doesn't exist: %s", top_path);
		return -1;
	}
	int result = get_paths_from_dir(d, top_path);
	closedir(d);
	return result;
}
