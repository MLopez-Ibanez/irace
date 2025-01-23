#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <stdarg.h>
#include <unistd.h>
#include "whereami.h"

#define MAX_PATH_LENGTH 1024

char *get_install_path(const char * package_path)
{
    char fullpath[MAX_PATH_LENGTH - 1] = "\0";
    int fullpath_length = 0;
    int res = wai_getExecutablePath(fullpath, MAX_PATH_LENGTH, &fullpath_length);
    if (res <= 0 || res <= fullpath_length)
        return NULL;
    // printf("fullpath: %s (%d)\n", fullpath, fullpath_length);
#ifdef WIN32
    for (size_t i = 0; i < fullpath_length; ++i) {
        if (fullpath[i] == '\\') {
            fullpath[i] = '/';
        }
    }
#endif
    // Remove package_path and everything after it from fullpath.
    size_t package_path_len = strlen(package_path);
    char *pos = strstr(fullpath, package_path);
    char * last_found = NULL;
    if (pos == NULL) {
        printf("Error: irace is installed at an unexpected path: %s\n", fullpath);
        exit(EXIT_FAILURE);
    }
    // Search the last occurrence in case fullpath is something like
    // /irace/bin/R/x86_64-pc-linux-gnu-library/4.1/irace/bin/irace .
    do  {
        last_found = pos;
        pos = strstr(pos + package_path_len, package_path);
    } while (pos != NULL);
    *last_found = '\0';
    fullpath_length = strlen(fullpath);
    char * path = (char*) malloc((fullpath_length + 1) * sizeof(char));
    memcpy(path, fullpath, fullpath_length + 1);
    // printf("path: %s (%d)\n", fullpath, fullpath_length);
    return path;
}
#undef MAX_PATH_LENGTH

int exec_R(int argc, char *argv[], const char * user_code)
{
    const char * path = get_install_path("/irace/bin/");
    if (path == NULL)
        path = "";

#define _libpath_str ".libPaths(c(r'{%s}', .libPaths()));%s"
#ifdef WIN32
// Windows execvp() does not pass command-line arguments correctly if they
// contain space, tab, backslash, or double-quote characters.
#define libpath_str "\"" _libpath_str "\""
#else
#define libpath_str _libpath_str
#endif
    // -4 because of two times %s.
    char * r_code = malloc(sizeof(char) * (strlen(libpath_str) - 4 + strlen(path) + strlen(user_code) + 1));
    sprintf(r_code, libpath_str, path, user_code);
#undef _libpath_str
#undef libpath_str
    // printf("%s\n", r_code);

    char * const extra_argv[] = { "--vanilla", "--slave", "-e", r_code, "--args"};
    int extra_argc = sizeof(extra_argv) / sizeof(char *);
    char **cmd_args = malloc((extra_argc + argc + 1) * sizeof(char *));
    cmd_args[0] = argv[0];
    for (int i = 0; i < extra_argc; i++)
        cmd_args[i+1] = extra_argv[i];
    for (int i = 1; i < argc; i++)
        cmd_args[extra_argc + i] = argv[i];
    cmd_args[extra_argc + argc] = NULL;
    // for (int i = 0; i < extra_argc + argc; i++)
    //     printf("%d: %s\n", i, cmd_args[i]);

#ifdef WIN32
# define EXE_EXT ".exe"
#else
# define EXE_EXT ""
#endif
    const char file[] = "R" EXE_EXT;
#undef EXE_EXT
    if (execvp(file, cmd_args) == -1) {
        perror("Error executing R");
        return EXIT_FAILURE;
    }
    return EXIT_SUCCESS;
}
