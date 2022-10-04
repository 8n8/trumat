#include <stdio.h>
#include <stdint.h>
#include <dirent.h>
#include <errno.h>

#define MAX_PATHS 200
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

    if (dir_name[0] == '.' && dir_name[1] == '.' && dir_name[2] == '\0') {
        return 0;
    }

    return 1;
}

int get_paths_from_dir(DIR* d, char directory_path[MAX_PATH]) {

	errno = 0;
	struct dirent* dir;
	while (1) {
		dir = readdir(d);
		if (dir == NULL && errno != 0) {
            printf("couldn't read directory: %s", directory_path);
			return -1;
		}
		if (dir == NULL && errno == 0) {
			return 0;
		}

		if (dir->d_type == DT_REG) {
            char file_path[MAX_PATH];
			make_file_path(file_path, directory_path, dir->d_name);
            for (int i = 0; i < MAX_PATH; ++i) {
                printf("%c", file_path[i]);
            }
            printf("\n");
		}

        if (dir->d_type == DT_DIR && !is_relevant_dir(dir->d_name)) {
            continue;
        }

		if (dir->d_type == DT_DIR) {
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
		}
	}
	return 0;
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
