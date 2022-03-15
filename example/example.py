from ctypes import *
import libcalc
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

        print('')
        print(result.value.decode('utf-8'))
        print('')

        libcalc.lisp_release_handle(expr)
        libcalc.lisp_release_handle(simplified)
