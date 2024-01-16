#ifdef _WIN32
#include <Windows.h>
#include <Process.h>
#include <psapi.h>
#else
#include <dlfcn.h>
#endif
#include <stdio.h>

extern int initialize_lisp(int argc, char *argv[], char *envp[]);

int (*funcall0_by_name)(const char *name);
void (*set_argv)(int argc, char **argv);
void (*load_array)(char *data, int size);
void (*load_shared_object)(char *pathname);

#define BUF_SIZE 1024

#ifdef _WIN32
BOOL WINAPI DllMain(HINSTANCE hinstDLL, DWORD fdwReason, LPVOID lpvReserved)
{
    if (fdwReason == DLL_PROCESS_ATTACH) {
        char lib_path[BUF_SIZE];
        GetModuleFileNameA(hinstDLL, lib_path, BUF_SIZE);
        char *init_args[] = {"", "--core", lib_path, "--no-userinit", "--noinform"};

        return !initialize_lisp(sizeof(init_args) / sizeof(init_args[0]), init_args, NULL);
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

    initialize_lisp(sizeof(init_args) / sizeof(init_args[0]), init_args, NULL);
}
#endif
