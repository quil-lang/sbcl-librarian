.PHONY: all clean

all: libcalc.dylib

libcalc.core libcalc.c: libcalc.lisp
	sbcl --script "libcalc.lisp"

libcalc.dylib: libcalc.core libcalc.c
	$(CC) -dynamiclib -o $@ libcalc.c \
	      -lsbcl
clean:
	rm -f libcalc.c libcalc.h libcalc.core libcalc.dylib
