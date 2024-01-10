#include <dlfcn.h>
#include <stdio.h>

extern int initialize_lisp(int argc, char *argv[], char *envp[]);

__attribute__((constructor))
void init(void)
{
    Dl_info info;

    dladdr(init, &info);
    printf("%s\n", info.dli_fname);
    const char *init_args[] = {"", "--core", info.dli_fname, "--no-userinit"};
    int res = initialize_lisp(4, init_args, NULL);
    printf("hello from library load time, initialize_lisp returned %d\n", res);
}
