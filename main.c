#include <stdio.h>
#include <stdint.h>
#include <dirent.h>
#include <errno.h>

enum Result {
	Ok,
	DirectoryDoesntExist,
	CouldntReadDirectory,
	TooManyFiles,
};

#define MAX_PATHS 200
#define MAX_PATH 4096

void save_file_path(
	char paths[MAX_PATH][MAX_PATHS],
	int* num,
	char directory_path[MAX_PATH],
	char file_name[256]) {

	// copy the directory path
	int i = 0;
	for (; i < MAX_PATH; ++i) {
		if (directory_path[i] == '\n') {
			break;
		}
		paths[(*num) + 1][i] = directory_path[i];
	}

	paths[(*num) + 1][i] = '/';
	++i;
	
	// copy the file name
	for (int j = 0; j < 256; ++j) {
		if (file_name[j] == '\0') {
			(*num)++;
			return;
		}

		paths[(*num) + 1][i + j] = file_name[j];
	}
}

void make_sub_dir_path(
	char sub_dir_path[MAX_PATH],
	char directory_path[MAX_PATH],
	char sub_dir_name[256]) {

	int i = 0;
	for (; i < MAX_PATH; ++i) {
		if (sub_dir_path[i] == '\0') {
			break;
		}

		sub_dir_path[i] = directory_path[i];
	}

	sub_dir_path[i] = '/';
	++i;

	for (int j = 0; j < 256; ++j) {
		if (sub_dir_name == NULL) {
			return;
		}
		sub_dir_path[i + j] = sub_dir_name[j];
	}
}

int get_paths_from_dir(
	char paths[MAX_PATH][MAX_PATHS],
	int* num,
	DIR* d,
	char directory_path[MAX_PATH]) {

	if ((*num) == MAX_PATH) {
		return TooManyFiles;
	}

	errno = 0;
	struct dirent* dir;
	while (1) {
		dir = readdir(d);
		if (dir == NULL && errno != 0) {
			return CouldntReadDirectory;
		}
		if (dir == NULL && errno == 0) {
			return Ok;
		}

		if (dir->d_type == DT_REG) {
			save_file_path(
				paths,
				num,
				directory_path,
				dir->d_name);
		}

		if (dir->d_type == DT_DIR) {
			char sub_dir_path[MAX_PATH];
			make_sub_dir_path(
				sub_dir_path,
				directory_path,
				dir->d_name);

			DIR* subd = opendir(sub_dir_path);
			if (d == NULL) {
				return DirectoryDoesntExist;
			}
			int result = get_paths_from_dir(
				paths,
				num,
				subd,
				sub_dir_path);
			closedir(subd);
			if (result != Ok) {
				return result;
			}
		}
	}
	return Ok;
}

int main(int argc, char* argv[]) {
	DIR* d;
	char top_path[MAX_PATH];
	top_path[0] = '.';
	top_path[1] = '\0';
	d = opendir(top_path);
	if (d == NULL) {
		return DirectoryDoesntExist;
	}
	char paths[MAX_PATH][MAX_PATHS];
	int num = 0;
	int result = get_paths_from_dir(paths, &num, d, top_path);
	closedir(d);

	return result;
}
