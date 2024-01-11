#include <stdio.h>
#include <string.h>

extern void (*funcall0_by_name)(const char *name);
extern void (*set_argv)(int argc, char **argv);
extern void (*load_array)(char *data, int size);

static char src[] = "(defun test () (format t \"i was loaded from a string~%\"))";

int main(int argc, char **argv)
{
    funcall0_by_name("PRINT-COMMAND-LINE-ARGS");
    set_argv(argc, argv);
    funcall0_by_name("PRINT-COMMAND-LINE-ARGS");
    load_array(src, strlen(src));
    funcall0_by_name("TEST");

    return 0;
}
