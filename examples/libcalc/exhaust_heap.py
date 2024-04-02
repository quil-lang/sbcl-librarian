import libcalc
import sys


if __name__ == '__main__':
    result = libcalc.calc_exhaust_heap()
    # Attempting to call into Lisp again should return the same error code
    assert(libcalc.calc_exhaust_heap() == result)
    if result == 2:
        print("returned to Python with error code %d after heap exhaustion" % result)
