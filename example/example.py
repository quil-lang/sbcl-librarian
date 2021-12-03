from ctypes import *
import libcalc

def die(msg):
    print(msg)
    exit(1)

if __name__ == '__main__':
    print("> ", end='')
    s = input()

    expr = libcalc.expr_type()
    if (libcalc.calc_parse(s.encode('utf-8'), byref(expr)) != 0):
        die("unable to parse expression")

    simplified = libcalc.expr_type()
    libcalc.calc_simplify(expr, byref(simplified))

    result = c_char_p()
    if (libcalc.calc_expression_to_string(simplified, byref(result)) != 0):
        die("unable to print expression to string")

    print('')
    print(result.value.decode('utf-8'))
    print('')
    
    libcalc.calc_release_handle(expr)
    libcalc.calc_release_handle(simplified)
    
