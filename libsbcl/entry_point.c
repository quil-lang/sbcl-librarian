#ifdef _WIN32
#include <Windows.h>
#include <Process.h>
#include <psapi.h>
#else
#include <dlfcn.h>
#endif
#include <stdlib.h>
#include <string.h>

#define BUF_SIZE 1024

extern char *sbcl_runtime_home;
extern char *sbcl_runtime;
extern char *dir_name(char *path);
extern int initialize_lisp(int argc, char *argv[], char *envp[]);

#ifdef _WIN32
BOOL WINAPI DllMain(HINSTANCE hinstDLL, DWORD fdwReason, LPVOID lpvReserved)
{
    if (fdwReason == DLL_PROCESS_ATTACH) {
        char lib_path[BUF_SIZE];
        GetModuleFileNameA(hinstDLL, lib_path, BUF_SIZE);
        char *init_args[] = {"", "--core", lib_path, "--no-userinit", "--noinform"};

        initialize_lisp(sizeof(init_args) / sizeof(init_args[0]), init_args, 0);

        int n = strlen(lib_path) * sizeof(char);
        sbcl_runtime = malloc(n);
        memcpy(sbcl_runtime, lib_path, n);
        sbcl_runtime_home = dir_name(sbcl_runtime);
    }

    return TRUE;
}
#else
__attribute__((constructor))
void init(void)
{
    Dl_info info;
    dladdr(init, &info);
    char *init_args[] = {"", "--core", info.dli_fname, "--no-userinit", "--noinform"};

    initialize_lisp(sizeof(init_args) / sizeof(init_args[0]), init_args, 0);

    int n = strlen(info.dli_fname) * sizeof(char);
    sbcl_runtime = malloc(n);
    memcpy(sbcl_runtime, info.dli_fname, n);
    sbcl_runtime_home = dir_name(sbcl_runtime);
}
#endif
