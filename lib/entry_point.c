#define LIBSBCL_LIBRARIAN_API_BUILD
#ifdef __linux__
#define _GNU_SOURCE
#endif

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "sbcl_librarian.h"

#ifdef _WIN32
# include <Windows.h>
# include <Process.h>
# include <psapi.h>

__thread intptr_t _fatal_lisp_error_handler[5];
intptr_t *fatal_lisp_error_handler(void) {
    return _fatal_lisp_error_handler;
}

__thread struct unwind_context lisp_calling_context;
#else
# include <dlfcn.h>

__thread jmp_buf fatal_lisp_error_handler;
#endif

#define BUF_SIZE 1024

extern char *dir_name(char *path);
extern void set_lossage_handler(void (*handler)(void));
extern int initialize_lisp(int argc, const char *argv[], char *envp[]);

int fatal_sbcl_error_occurred = 0;
int initialized = 0;

static void return_from_lisp(void)
{
    fatal_sbcl_error_occurred = 1;
    fflush(stdout);
    fflush(stderr);
    longjmp(fatal_lisp_error_handler, 1);
}

static void do_initialize_lisp(const char *libsbcl_librarian_path)
{
    char *libsbcl_librarian_dir = dir_name(libsbcl_librarian_path);
    int libsbcl_librarian_dir_len = strlen(libsbcl_librarian_dir);
    int core_path_size = libsbcl_librarian_dir_len + sizeof("sbcl_librarian.core") + 1;
    char *core_path = malloc(core_path_size);

    snprintf(core_path, core_path_size, "%ssbcl_librarian.core", libsbcl_librarian_dir);

    const char *init_args[] = {"", "--dynamic-space-size", "8192", "--core", core_path, "--noinform", "--no-userinit"};

    /*
     * It seems that on Linux, dlsym(NULL, "sym") fails to find "sym"
     * unless its containing shared object was explicitly dlopened
     * with the RTLD_GLOBAL flag. The process has already loaded both
     * libsbcl.so and libsbcl_librarian.so at this point, so this is a
     * no-op except for making their symbols visible in the global
     * namespace. Without this, initialize_lisp will fail when it
     * tries to initialize the alien callable symbols defined in the
     * core file.
     */
#ifdef __linux__
    dlopen("libsbcl.so", RTLD_NOW | RTLD_GLOBAL);
    dlopen(libsbcl_librarian_path, RTLD_NOW | RTLD_GLOBAL);
#endif
    initialize_lisp(sizeof(init_args) / sizeof(init_args[0]), init_args, NULL);
    initialized = 1;
    set_lossage_handler(return_from_lisp);
}

#ifdef _WIN32
BOOL WINAPI DllMain(HINSTANCE hinstDLL, DWORD fdwReason, LPVOID lpvReserved)
{
    if (fdwReason == DLL_PROCESS_ATTACH) {
        char libsbcl_librarian_path[BUF_SIZE];

        GetModuleFileNameA(hinstDLL, libsbcl_librarian_path, BUF_SIZE);
        do_initialize_lisp(libsbcl_librarian_path);
        lisp_calling_context_tls_index = TlsAlloc();
    } else if (fdwReason == DLL_THREAD_ATTACH) {
        TlsSetValue(lisp_calling_context_tls_index, &lisp_calling_context);
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
