import os
import platform

# Python has trouble finding some dependent DLLs in the Windows CI
# environment, so we register their directories directly
if platform.system() == 'Windows':
    os.add_dll_directory(os.environ.get('LIBSBCL_PATH'))
    os.add_dll_directory(os.environ.get('MINGW64_PATH'))

import sbcl_librarian.wrapper
import calc as libcalc
import sys


if __name__ == '__main__':
    # Attempting to call into Lisp again should throw the same exception
    for i in range(2):
        try:
            libcalc.calc_exhaust_heap()
        except sbcl_librarian.wrapper.LispFatal:
            print("returned to Python after heap exhaustion (attempt %d)" % i)
