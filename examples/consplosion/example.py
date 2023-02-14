from ctypes import *
import consplosion
import sys

def die(msg):
    print(msg)
    exit(1)

def accept_command():
        [command, arg] = [x for x in input().split(' ') if x != '']
        return command, int(arg)
    
if __name__ == '__main__':
    while True:
        print("consplode > ", end='')
        try:
            (command, arg) = accept_command()
        except ValueError:
            print("Bad input, try again")
            continue
        except EOFError:
            sys.exit(0)

        consbomb = consplosion.consbomb_type()
        if command == 'cons':
            result = consplosion.consplode(arg, byref(consbomb))
        elif command == 'hashtab':
            result = consplosion.consplode_hashtables(arg, byref(consbomb))
        elif command == 'vector':
            result = consplosion.consplode_vector(arg, byref(consbomb))
        else:
            print("Unknown command " + command)


        if result != 0:
            print("conspbomb exploded")
            sys.exit(0)
        else:
            print("consbomb still ticking...")
            

        consplosion.lisp_release_handle(consbomb)


