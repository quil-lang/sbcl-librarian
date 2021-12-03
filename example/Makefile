.PHONY: all clean

all: libcalc.dylib

libcalc.core libcalc.c libcalc.h libcalc.py: libcalc.lisp
	$(SBCL_SRC)/run-sbcl.sh --script "libcalc.lisp"

libcalc.dylib: libcalc.core libcalc.c
	$(CC) -dynamiclib -o $@ libcalc.c -L$(SBCL_SRC)/src/runtime -lsbcl
clean:
	rm -f libcalc.c libcalc.h libcalc.core libcalc.py libcalc.dylib
