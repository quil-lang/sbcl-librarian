#define LIBSBCL_API_BUILD
#ifdef __linux__
#define _GNU_SOURCE
#endif

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

#include "libsbcl_librarian.h"

#define BUF_SIZE 1024

extern char *sbcl_runtime_home;
extern char *sbcl_runtime;
extern char *dir_name(char *path);
extern int initialize_lisp(int argc, const char *argv[], char *envp[]);

static void do_initialize_lisp(const char *libsbcl_path)
{
    char *libsbcl_dir = dir_name(libsbcl_path);
    int libsbcl_dir_len = strlen(libsbcl_dir);
    int core_path_size = libsbcl_dir_len + sizeof("libsbcl_librarian.core") + 1;
    char *core_path = malloc(core_path_size);

    snprintf(core_path, core_path_size, "%slibsbcl_librarian.core", libsbcl_dir);

    const char *init_args[] = {"", "--dynamic-space-size", "8192", "--core", core_path, "--noinform", "--no-userinit"};

    initialize_lisp(sizeof(init_args) / sizeof(init_args[0]), init_args, NULL);

    int sbcl_home_path_size = libsbcl_dir_len + sizeof("sbcl") + 1;
    int libsbcl_path_size = strlen(libsbcl_path) + 1;
    sbcl_runtime = malloc(libsbcl_path_size);
    strncpy(sbcl_runtime, libsbcl_path, libsbcl_path_size);
    sbcl_runtime_home = malloc(sbcl_home_path_size);
    snprintf(sbcl_runtime_home, sbcl_home_path_size, "%ssbcl", libsbcl_dir);
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
static void init(void)
{
    Dl_info info;

    dladdr(do_initialize_lisp, &info);
    do_initialize_lisp(info.dli_fname);
}
#endif
