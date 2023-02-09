import os
from ctypes import *
from ctypes.util import find_library
from pathlib import Path

try:
    libpath = Path(find_library('libcalc')).resolve()
except TypeError as e:
    raise Exception('Unable to locate libcalc') from e

libcalc = CDLL(str(libpath), mode=RTLD_GLOBAL)

libcalc.init(str(libpath.parent / 'libcalc.core').encode('utf-8'))


lisp_release_handle = CFUNCTYPE(None, c_void_p)(c_void_p.in_dll(libcalc, 'lisp_release_handle').value)
lisp_handle_eq = CFUNCTYPE(c_bool, c_void_p, c_void_p)(c_void_p.in_dll(libcalc, 'lisp_handle_eq').value)
lisp_enable_debugger = CFUNCTYPE(None, )(c_void_p.in_dll(libcalc, 'lisp_enable_debugger').value)
lisp_disable_debugger = CFUNCTYPE(None, )(c_void_p.in_dll(libcalc, 'lisp_disable_debugger').value)
lisp_gc = CFUNCTYPE(None, )(c_void_p.in_dll(libcalc, 'lisp_gc').value)
class expr_type(c_void_p):
    pass


class err_t(int):
    _map = {
        0: "ERR_SUCCESS",
        1: "ERR_FAIL",
    }


calc_int_literal = CFUNCTYPE(err_t, c_int, POINTER(expr_type))(c_void_p.in_dll(libcalc, 'calc_int_literal').value)
calc_int_literal_value = CFUNCTYPE(err_t, expr_type, POINTER(c_int))(c_void_p.in_dll(libcalc, 'calc_int_literal_value').value)
calc_int_literal_p = CFUNCTYPE(err_t, expr_type, POINTER(c_bool))(c_void_p.in_dll(libcalc, 'calc_int_literal_p').value)
calc_sum_expression = CFUNCTYPE(err_t, expr_type, expr_type, POINTER(expr_type))(c_void_p.in_dll(libcalc, 'calc_sum_expression').value)
calc_sum_expression_left_arg = CFUNCTYPE(err_t, expr_type, POINTER(expr_type))(c_void_p.in_dll(libcalc, 'calc_sum_expression_left_arg').value)
calc_sum_expression_right_arg = CFUNCTYPE(err_t, expr_type, POINTER(expr_type))(c_void_p.in_dll(libcalc, 'calc_sum_expression_right_arg').value)
calc_sum_expression_p = CFUNCTYPE(err_t, expr_type, POINTER(c_bool))(c_void_p.in_dll(libcalc, 'calc_sum_expression_p').value)
calc_simplify = CFUNCTYPE(err_t, expr_type, POINTER(expr_type))(c_void_p.in_dll(libcalc, 'calc_simplify').value)
calc_parse = CFUNCTYPE(err_t, c_char_p, POINTER(expr_type))(c_void_p.in_dll(libcalc, 'calc_parse').value)
calc_expression_to_string = CFUNCTYPE(err_t, expr_type, POINTER(c_char_p))(c_void_p.in_dll(libcalc, 'calc_expression_to_string').value)
calc_remove_zeros = CFUNCTYPE(err_t, expr_type, POINTER(expr_type))(c_void_p.in_dll(libcalc, 'calc_remove_zeros').value)


__all__ = ['lisp_release_handle', 'lisp_handle_eq', 'lisp_enable_debugger', 'lisp_disable_debugger', 'lisp_gc', 'expr_type', 'err_t', 'calc_int_literal', 'calc_int_literal_value', 'calc_int_literal_p', 'calc_sum_expression', 'calc_sum_expression_left_arg', 'calc_sum_expression_right_arg', 'calc_sum_expression_p', 'calc_simplify', 'calc_parse', 'calc_expression_to_string', 'calc_remove_zeros']

