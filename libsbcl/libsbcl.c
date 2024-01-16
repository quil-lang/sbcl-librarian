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

#ifdef _WIN32
void NtPathToDosPath(char *pszFilename) {
    // Translate path with device name to drive letters.
    TCHAR szTemp[BUFSIZE];
    szTemp[0] = '\0';

    if (GetLogicalDriveStrings(BUFSIZE-1, szTemp))
    {
        TCHAR szName[MAX_PATH];
        TCHAR szDrive[3] = TEXT(" :");
        BOOL bFound = FALSE;
        TCHAR* p = szTemp;

        do
        {
            // Copy the drive letter to the template string
            *szDrive = *p;

            // Look up each device name
            if (QueryDosDevice(szDrive, szName, MAX_PATH))
            {
                size_t uNameLen = _tcslen(szName);

                if (uNameLen < MAX_PATH)
                {
                    bFound = _tcsnicmp(pszFilename, szName, uNameLen) == 0
                        && *(pszFilename + uNameLen) == _T('\\');

                    if (bFound)
                    {
                        // Reconstruct pszFilename using szTempFile
                        // Replace device path with DOS path
                        TCHAR szTempFile[MAX_PATH];
                        StringCchPrintf(szTempFile,
                                MAX_PATH,
                                TEXT("%s%s"),
                                szDrive,
                                pszFilename+uNameLen);
                        StringCchCopyN(pszFilename, MAX_PATH+1, szTempFile, _tcslen(szTempFile));
                    }
                }
            }

            // Go to the next NULL character.
            while (*p++);
        } while (!bFound && *p); // end of string
    }
}
#endif

__attribute__((constructor))
void init(void)
{
   char *lib_path;
#ifdef _WIN32
   char buf[LIB_PATH_BUF_SIZE];

   GetMappedFileNameA(GetCurrentProcess(), init, buf, BUF_SIZE);
   NtPathToDosPath(buf);
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
