from ctypes import *
import consplosion
import sys

def die(msg):
    print(msg)
    exit(1)

if __name__ == '__main__':
    while True:
        print("consplode > ", end='')
        try:
            n = int(input())
        except EOFError:
            sys.exit(0)

        consbomb = consplosion.consbomb_type()
        if (consplosion.consplode(n, byref(consbomb)) != 0):
            die("consbomb exploded :(")
        else:
            print("consbomb still ticking...")
            
        # result = c_char_p()
        # if (libcalc.calc_expression_to_string(simplified, byref(result)) != 0):
        #     die("unable to print expression to string")

        # print('')
        # print(result.value.decode('utf-8'))
        # print('')

        consplosion.lisp_release_handle(consbomb)


