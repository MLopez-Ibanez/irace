#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <stdarg.h>
#include <unistd.h>
#include "whereami.h"

#define max_path_length 1024
char *get_install_path(const char * package_path)
{
    char fullpath[max_path_length - 1] = "\0";
    int dirname_length = 0;
    int res = wai_getExecutablePath(fullpath, max_path_length, &dirname_length);
    if (res <= 0)
        return NULL;
    if (res <= dirname_length)
        return NULL;
    dirname_length -= strlen(package_path);
    char * path = (char*) malloc(dirname_length+1);
    memcpy(path, fullpath, dirname_length);
    path[dirname_length] = '\0';
    return path;
}

int exec_R(int argc, char *argv[], const char * user_code)
{
#ifdef WIN32
# define EXE_EXT ".exe"
#else
# define EXE_EXT ""
#endif
    const char file[] = "R" EXE_EXT;
    const char * path = get_install_path("/irace/bin");
    if (path == NULL) path = "";
#define libpath_str ".libPaths(c('%s', .libPaths()));%s"
    char * r_code = malloc(sizeof(char) * (strlen(libpath_str) - 4 + strlen(path) + strlen(user_code) + 1));
    sprintf(r_code, libpath_str, path, user_code);
    //printf("%s\n", libpath);
    
    char * const extra_argv[] = { "--vanilla", "--slave", "-e", r_code, "--args"};
    int extra_argc = sizeof(extra_argv) / sizeof(char *);
    char **cmd_args = calloc(extra_argc + argc + 1, sizeof(char *));
    cmd_args[0] = argv[0];
    for (int i = 0; i < extra_argc; i++)
        cmd_args[i+1] = extra_argv[i];
    for (int i = 1; i < argc; i++)
        cmd_args[extra_argc + i] = argv[i];
    return execvp(file, cmd_args);
}
