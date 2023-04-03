from ctypes import *
import libcallback
import sys


@CFUNCTYPE(None, c_char_p, POINTER(c_char))
def foo(s, outbuffer):
    writable = cast(outbuffer, POINTER(c_char * 100))
    writable.contents.value = (s.decode("utf-8") + " it works!").encode("utf-8")

def moo():
    raw_str = create_string_buffer(100)
    libcallback.callback_call_callback(foo, raw_str)
    return raw_str.value.decode("utf-8")

if __name__ == '__main__':
    print(moo())
