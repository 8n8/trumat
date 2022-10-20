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

struct CodeBuffers CODE_BUFFERS;

int is_start_verbatim_string(
	int i,
	char buf[CODE_BUF_SIZE],
	int size) {

	return
		size - i > 3 &&
		buf[i] == '"' &&
		buf[i+1] == '"' &&
		buf[i+2] == '"';
}

int is_start_of_type_declaration(int i, char buf[CODE_BUF_SIZE], int size) {
	return
		size - i > 6 &&
		buf[i] == ' ' &&
		buf[i-1] == 'e' &&
		buf[i-2] == 'p' &&
		buf[i-3] == 'y' &&
		buf[i-4] == 't' &&
		buf[i-5] == '\n';
}

int is_ending_verbatim_string(
	int i,
	char buf[CODE_BUF_SIZE],
	int size) {

	return
		i > 2 &&
		buf[i] == '"' &&
		buf[i-1] == '"' &&
		buf[i-2] == '"' &&
		buf[i-3] != '\\';
}

int is_ending_block_comment(int i, char buf[CODE_BUF_SIZE], int size) {
	return i > 0 && buf[i] == '}' && buf[i-1] == '-';
}

int is_name_char(char ch) {
	return
		(ch >= 'a' && ch <= 'z') ||
		(ch >= 'A' && ch <= 'Z') ||
		(ch >= '0' && ch <= '9') ||
		ch == '_';
}

int is_ending_let(int i, char buf[CODE_BUF_SIZE], int size) {
	return
		i > 3 &&
		buf[i] == 'n' &&
		buf[i-1] == 'i' &&
		!is_name_char(buf[i-2]) &&
		i+1 < size &&
		(buf[i+1] == ' ' || buf[i+1] == '\n');
}

int is_ending_type_declaration(int i, char buf[CODE_BUF_SIZE], int size) {
	return i > 1 && buf[i] != ' ' && buf[i-1] == '\n';
}

enum SubRegion {
	Let,
	NotInSubRegion,
	CurlyBracketCollection,
	BracketCollection,
	ParenthesisCollection,
	BlockComment,
	VerbatimString,
	TypeDeclaration,
};

int end_of_sub_region(
	int i,
	enum SubRegion sub_region,
	int size,
	char buf[CODE_BUF_SIZE]) {

	switch (sub_region) {
	case TypeDeclaration:
		return is_ending_type_declaration(i, buf, size);
	case VerbatimString:
		return is_ending_verbatim_string(i, buf, size);
	case BlockComment:
		return is_ending_block_comment(i, buf, size);
	case ParenthesisCollection:
		return buf[i] == ')';
	case BracketCollection:
		return buf[i] == ']';
	case CurlyBracketCollection:
		return buf[i] == '}';
	case Let:
		return is_ending_let(i, buf, size);
	case NotInSubRegion:
		return 0;
	}
}

int is_start_block_comment(int i, char buf[CODE_BUF_SIZE], int size) {
	return
		size - i > 2 &&
		buf[i] == '{' &&
		buf[i+1] == '-';
}

int is_start_let(int i, char buf[CODE_BUF_SIZE], int size) {
	return
		i > 0 &&
		size - i > 4 &&
        (buf[i-1] == ' ' || buf[i-1] == '\n') &&
		buf[i] == 'l' &&
		buf[i+1] == 'e' &&
		buf[i+2] == 't' &&
		(buf[i+3] == ' ' || buf[i+3] == '\n');
}

enum SubRegion start_of_sub_region(int i, int size, char buf[CODE_BUF_SIZE]) {
	if (is_start_of_type_declaration(i, buf, size)) {
		return TypeDeclaration;
	}

	if (is_start_verbatim_string(i, buf, size)) {
		return VerbatimString;
	}

	if (is_start_block_comment(i, buf, size)) {
		return BlockComment;
	}

	if (buf[i] == '[') {
		return BracketCollection;
	}

	if (buf[i] == '{') {
		return CurlyBracketCollection;
	}

	if (buf[i] == '(') {
		return ParenthesisCollection;
	}

	if (is_start_let(i, buf, size)) {
		return Let;
	}

	return NotInSubRegion;
}

int consume_spaces(char buf[CODE_BUF_SIZE], int i, int size) {
	for (; i < size && buf[i] == ' '; ++i) {}
	return i;
}

int equals_but_not_at_end_of_line(char buf[CODE_BUF_SIZE], int i, int size) {
	return
		i > 0 &&
		buf[i+1] != '=' &&
		buf[i-1] != '=' &&
		buf[i-1] != '!' &&
		buf[i-1] != '<' &&
		buf[i-1] != '>' &&
		buf[i] == '=' &&
		buf[consume_spaces(buf, i+1, size)] != '\n';
}

void toplevel_body_indent(
	char one[CODE_BUF_SIZE],
	char two[CODE_BUF_SIZE],
	int* one_size,
	int* two_size) {

	enum SubRegion sub_region_status = NotInSubRegion;
	int sub_region_nesting = 0;

	int two_i = 0;
	for (int one_i = 0; one_i < *one_size; ++one_i) {
		enum SubRegion new_sub_region_status =
			start_of_sub_region(one_i, *one_size, one);

		if (
			sub_region_status != NotInSubRegion &&
			new_sub_region_status == sub_region_status) {

			++sub_region_nesting;
		}

		if (sub_region_status == NotInSubRegion) {
			sub_region_status = new_sub_region_status;
		}

		if (end_of_sub_region(
			one_i,
			sub_region_status,
			*one_size,
			one)) {

			if (sub_region_nesting == 0) {
				sub_region_status = NotInSubRegion;
			} else {
				--sub_region_nesting;
			}
		}

		if (
			sub_region_status == NotInSubRegion &&
			one[one_i] == '=' &&
			one_i < *one_size &&
			one[one_i + 1] != '=' &&
			one_i > 0 &&
			one[one_i - 1] != '=') {

			two[two_i] = '=';
			++two_i;
			two[two_i] = '\n';
			++two_i;

			one_i += 2;

			for (
				;
				one[one_i + 1] == ' ' ||
					one[one_i + 1] == '\n';
				++one_i) {
			}
			for (int i = 0; i < 3; ++i) {
				two[two_i] = ' ';
				++two_i;
			}
		}

		two[two_i] = one[one_i];
		++two_i;
	}
	*two_size = two_i;
}

void newline_after_toplevel_bind(
	char one[CODE_BUF_SIZE],
	char two[CODE_BUF_SIZE],
	int* one_size,
	int* two_size) {

	enum SubRegion sub_region_status = NotInSubRegion;
	int sub_region_nesting = 0;

	int two_i = 0;
	for (int one_i = 0; one_i < *one_size; ++one_i) {
		enum SubRegion new_sub_region_status =
			start_of_sub_region(one_i, *one_size, one);

		if (
			sub_region_status != NotInSubRegion &&
			new_sub_region_status == sub_region_status) {

			++sub_region_nesting;
		}

		if (sub_region_status == NotInSubRegion) {
			sub_region_status = new_sub_region_status;
		}


		if (end_of_sub_region(
			one_i,
			sub_region_status,
			*one_size,
			one)) {

			if (sub_region_nesting == 0) {
				sub_region_status = NotInSubRegion;
			} else {
				--sub_region_nesting;
			}
		}

		if (
			sub_region_status == NotInSubRegion &&
			equals_but_not_at_end_of_line(one, one_i, *one_size)) {

			++one_i;

			two[two_i] = '=';
			++two_i;
			two[two_i] = '\n';
			++two_i;

			for (
				;
				one[one_i + 1] == ' ' ||
					one[one_i + 1] == '\n';
				++one_i) {
			}

			for (int i = 0; i < 4; ++i) {
				two[two_i] = ' ';
				++two_i;
			}
		}

		two[two_i] = one[one_i];
		++two_i;
	}
	*two_size = two_i;
}

void no_trailing_space(
	char one[CODE_BUF_SIZE],
	char two[CODE_BUF_SIZE],
	int* one_size,
	int* two_size) {

	int num_consecutive_space = 0;

	int two_i = 0;
	for (int one_i = 0; one_i < *one_size; ++one_i) {
		if (one[one_i] == ' ') {
			num_consecutive_space++;
		}

		if (one[one_i] != ' ' && one[one_i] != '\n') {

			num_consecutive_space = 0;
		}

		if (one[one_i] == '\n' && num_consecutive_space > 0) {

			two_i = two_i - num_consecutive_space;
			num_consecutive_space = 0;
		}

		two[two_i] = one[one_i];
		two_i++;
	}

	*two_size = two_i;
}

int format_file(char path[MAX_PATH]) {
	printf("%s\n", path);

	FILE* handle_in = fopen(path, "rb");
	if (handle_in == NULL) {
		printf("failed to open the file: %s", path);
		return -1;
	}

	size_t n = fread(CODE_BUFFERS.one, 1, CODE_BUF_SIZE, handle_in);
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

	CODE_BUFFERS.one_size = n;

	// Add formatters in here.
	no_trailing_space(
		CODE_BUFFERS.one,
		CODE_BUFFERS.two,
		&CODE_BUFFERS.one_size,
		&CODE_BUFFERS.two_size);
	newline_after_toplevel_bind(
		CODE_BUFFERS.two,
		CODE_BUFFERS.one,
		&CODE_BUFFERS.two_size,
		&CODE_BUFFERS.one_size);
	toplevel_body_indent(
		CODE_BUFFERS.one,
		CODE_BUFFERS.two,
		&CODE_BUFFERS.one_size,
		&CODE_BUFFERS.two_size);

	FILE* handle_out = fopen(path, "w");
	if (handle_out == NULL) {
		printf("couldn't open output file: %s", path);
		return -1;
	}

	n = fwrite(CODE_BUFFERS.two, 1, CODE_BUFFERS.two_size, handle_out);
	if (n != CODE_BUFFERS.two_size) {
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
