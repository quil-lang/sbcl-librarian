from ctypes import *
import libsquare


def test_square():
    result = c_int()
    libsquare.square_square(-2, byref(result))
    assert result.value == 4
