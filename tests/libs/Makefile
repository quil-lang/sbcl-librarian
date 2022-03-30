.PHONY: all clean

all: $(LIB).dylib

$(LIB).core $(LIB).c $(LIB).h $(LIB).py: $(wildcard *.lisp) $(LIB).asd
	$(SBCL_SRC)/run-sbcl.sh --load script.lisp

$(LIB).dylib: $(LIB).core $(LIB).c
	$(CC) -dynamiclib -o $@ $(LIB).c -L$(SBCL_SRC)/src/runtime -lsbcl

clean:
	rm -f $(LIB).c $(LIB).h $(LIB).core $(LIB).py $(LIB).dylib
