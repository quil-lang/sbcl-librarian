import os
import platform

# Python has trouble finding some dependent DLLs in the Windows CI
# environment, so we register their directories directly
if platform.system() == 'Windows':
    os.add_dll_directory(os.environ.get('LIBSBCL_PATH'))
    os.add_dll_directory(os.environ.get('MINGW64_PATH'))

from ctypes import *
import calc as libcalc
import sbcl_librarian.raw
import sys

def die(msg):
    print(msg)
    exit(1)

if __name__ == '__main__':
    while True:
        print("> ", end='')
        try:
            s = input()
        except EOFError:
            sys.exit(0)

        expr = libcalc.expr_type()
        if (libcalc.calc_parse(s.encode('utf-8'), byref(expr)) != 0):
            die("unable to parse expression")

        simplified = libcalc.expr_type()

        if (len(sys.argv) == 2 and sys.argv[1] == 'remove-zeros'):
            libcalc.calc_remove_zeros(expr, byref(simplified))
        else:
            libcalc.calc_simplify(expr, byref(simplified))

        result = c_char_p()
        if (libcalc.calc_expression_to_string(simplified, byref(result)) != 0):
            die("unable to print expression to string")

        print(result.value.decode('utf-8'))

        sbcl_librarian.raw.lisp_release_handle(expr)
        sbcl_librarian.raw.lisp_release_handle(simplified)

