#include "irace.h"
int main(int argc, char *argv[])
{
    return exec_R(argc, argv, "library(irace);irace_cmdline()");
}


