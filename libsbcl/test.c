#include <stdio.h>

extern void (*funcall0_by_name)(const char *name);
extern void (*set_argv)(int argc, char **argv);

int main(int argc, char **argv)
{
    funcall0_by_name("PRINT-COMMAND-LINE-ARGS");
    set_argv(argc, argv);
    funcall0_by_name("PRINT-COMMAND-LINE-ARGS");
    printf("hello\n");

    return 0;
}
