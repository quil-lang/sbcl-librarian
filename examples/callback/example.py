from ctypes import *
import libcallback
import sys


@CFUNCTYPE(c_char_p)
def foo():
    return "it works".encode("utf-8")

def moo():
    raw_str = c_char_p()
    libcallback.callback_call_callback(foo, raw_str)
    return raw_str.value.decode("utf-8")

if __name__ == '__main__':
    print(moo())
