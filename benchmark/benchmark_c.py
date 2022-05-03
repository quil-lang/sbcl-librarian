import os
from ctypes import *
from ctypes.util import find_library
from pathlib import Path

try:
    libpath = Path(find_library('benchmark_c')).resolve()
except TypeError as e:
    raise Exception('Unable to locate benchmark') from e

benchmark = CDLL(str(libpath), mode=RTLD_GLOBAL)

class err_t(int):
    _map = {
        0: "ERR_SUCCESS",
        1: "ERR_FAIL",
    }

class opaque_c_type(c_void_p):
    pass

c_empty_function = CFUNCTYPE(err_t, )(c_void_p.in_dll(benchmark, 'c_empty_function').value)
c_opaque_function = CFUNCTYPE(err_t, POINTER(opaque_c_type))(c_void_p.in_dll(benchmark, 'c_opaque_function').value)
