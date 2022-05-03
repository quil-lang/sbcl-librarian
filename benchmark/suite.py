from ctypes import *
import benchmark
import benchmark_c
import sys
import time

def python_empty_function():
    return

def python_false_function():
    return False

def time_function(name, function, reps=20000):
    stime = time.time()
    for _ in range(10000):
        function()
    elapsed = time.time() - stime
    print(f"{f'timing {name} for {reps} repetitions:' :<60} {elapsed}")
    return

def print_table_header(header):
    print("\n")
    print(f"{header: ^80}")
    print(80*'-')

if __name__ == '__main__':
    print_table_header(f"Timing pure python overhead")
    time_function('void -> void',
                  lambda: python_empty_function())
    time_function('void -> False',
                  lambda: python_false_function())

    print_table_header("Timing python -> C overhead")
    time_function('void -> void',
                  lambda: benchmark_c.c_empty_function())
    opaque_object = byref(benchmark_c.opaque_c_type())
    time_function('void -> void*',
                  lambda: benchmark_c.c_opaque_function(opaque_object))

    print_table_header("Timing python -> C -> Lisp FFI overhead")
    time_function('void -> void',
                  lambda: benchmark.benchmark_empty_function())
    lisp_object = benchmark.lisp_type()
    time_function('void -> nil',
                  lambda: benchmark.benchmark_nil_function(byref(lisp_object)))
    benchmark.lisp_release_handle(lisp_object)
    lisp_object = benchmark.lisp_type()
    s = "123123".encode('utf-8')
    time_function('string -> lisp_obj',
                  lambda: benchmark.benchmark_string_function(s, byref(lisp_object)))
    time_function('lisp_obj -> lisp_obj',
                  lambda: benchmark.benchmark_identity_function(lisp_object, byref(lisp_object)))
