#ifdef _WIN32
#include <Windows.h>
#include <psapi.h>
#else
#include <dlfcn.h>
#endif
#include <stdio.h>

extern int initialize_lisp(int argc, char *argv[], char *envp[]);

int (*funcall0_by_name)(const char *name);
void (*set_argv)(int argc, char **argv);
void (*load_array)(char *data, int size);

#define BUF_SIZE 1024

__attribute__((constructor))
void init(void)
{
    char *lib_path;
#ifdef _WIN32
    char buf[BUF_SIZE];
    HMODULE mod;

    GetModuleHandleEx(
        GET_MODULE_HANDLE_EX_FLAG_FROM_ADDRESS | GET_MODULE_HANDLE_EX_FLAG_UNCHANGED_REFCOUNT,
        (LPCTSTR) init,
        &mod
    );
    GetModuleFileNameA(mod, buf, BUF_SIZE);
#if 0
    GetMappedFileNameA(GetCurrentProcess(), init, buf, BUF_SIZE);
#endif
    lib_path = buf;
#else
    Dl_info info;

    dladdr(init, &info);
    lib_path = info.dli_fname;
#endif
    printf("%s\n", lib_path);
    char *init_args[] = {"", "--core", lib_path, "--no-userinit", "--noinform"};
    int res = initialize_lisp(5, init_args, NULL);
    printf("hello from library load time, initialize_lisp returned %d\n", res);
}
