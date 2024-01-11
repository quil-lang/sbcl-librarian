#include <stdio.h>
#include <string.h>

#define INCBIN_STYLE INCBIN_STYLE_SNAKE
#define INCBIN_PREFIX
#include "incbin.h"

extern void (*funcall0_by_name)(const char *name);
extern void (*set_argv)(int argc, char **argv);
extern void (*load_array)(char *data, int size);

static char src[] = "(defun test () (format t \"i was loaded from a string. argv: ~A~%\" (uiop:command-line-arguments)))";

INCBIN(fasl, "test.fasl");

int main(int argc, char **argv)
{
    set_argv(argc, argv);
    load_array(src, strlen(src));
    load_array(fasl_data, fasl_size);
    funcall0_by_name("test");
    funcall0_by_name("test-fasl");

    return 0;
}
