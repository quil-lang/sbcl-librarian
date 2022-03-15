.PHONY: all clean

SBCL_SRC=$(HOME)/.roswell/src/sbcl-2.2.2

all: example

example: example.c libcalc.dylib libsbcl.so
	$(CC) example.c -L. -lsbcl -lcalc  -o $@

# When missing you need to run ./make-shared-library.sh in sbcl
# source.
#
# see: https://github.com/quil-lang/sbcl-librarian/issues/21#issuecomment-1068060794
libsbcl.so: $(SBCL_SRC)/src/runtime/libsbcl.so
	ln -s $< $@

libcalc.core libcalc.c libcalc.h libcalc.py: libcalc.lisp
	ros run -l "script.lisp" -q
#	$(SBCL) --script "script.lisp"

libcalc.dylib: libcalc.core libcalc.c libsbcl.so
	$(CC) -dynamiclib -o $@ libcalc.c -L. -lsbcl
clean:
	rm -f libcalc.c libcalc.h libcalc.core libcalc.py libcalc.dylib example
