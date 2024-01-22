#ifdef _WIN32
#include <Windows.h>
#include <Process.h>
#include <psapi.h>
#else
#include <dlfcn.h>
#endif
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "libsbcl.h"

#define BUF_SIZE 1024

extern char *sbcl_runtime_home;
extern char *sbcl_runtime;
extern char *dir_name(char *path);
extern int initialize_lisp(int argc, char *argv[], char *envp[]);

static void do_initialize_lisp(char *libsbcl_path)
{
    char *init_args[] = {"", "--core", libsbcl_path, "--no-userinit", "--noinform"};

    initialize_lisp(sizeof(init_args) / sizeof(init_args[0]), init_args, 0);

    int libsbcl_path_len = strlen(libsbcl_path);

    sbcl_runtime = malloc((libsbcl_path_len + 1) * sizeof(char));
    strlcpy(sbcl_runtime, libsbcl_path, libsbcl_path_len + 1);

    char *libsbcl_dir = dir_name(sbcl_runtime);
    int libsbcl_dir_len = strlen(libsbcl_dir);
    int sbcl_home_path_len = libsbcl_dir_len + sizeof("libsbcl");

    sbcl_runtime_home = malloc((sbcl_home_path_len + 1) * sizeof(char));
    snprintf(sbcl_runtime_home, sbcl_home_path_len, "%slibsbcl", libsbcl_dir);

    lisp_funcall0_by_name("os-cold-init-or-reinit", "sb-sys");
}

#ifdef _WIN32
BOOL WINAPI DllMain(HINSTANCE hinstDLL, DWORD fdwReason, LPVOID lpvReserved)
{
    if (fdwReason == DLL_PROCESS_ATTACH) {
        char libsbcl_path[BUF_SIZE];

        GetModuleFileNameA(hinstDLL, libsbcl_path, BUF_SIZE);
        do_initialize_lisp(libsbcl_path);
    }

    return TRUE;
}
#else
__attribute__((constructor))
void init(void)
{
    Dl_info info;

    dladdr(do_initialize_lisp, &info);
    do_initialize_lisp(info.dli_fname);
}
#endif
