#include <stdio.h>
#include <string.h>

#include <libsbcl.h>

#define INCBIN_STYLE INCBIN_STYLE_SNAKE
#define INCBIN_PREFIX
#include "incbin.h"

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
