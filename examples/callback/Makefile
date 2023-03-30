EXAMPLES_DIR := $(shell pwd)
ROOT_DIR := $(shell dirname $(shell dirname $(EXAMPLES_DIR)))


.PHONY: all clean

all: libsbcl.so libcallback.dylib

# For some reason, even on OSX SBCL builds its library with *.so
# extension, but this works!
libsbcl.so: $(SBCL_SRC)
	test ! -e $(SBCL_SRC)/src/runtime/libsbcl.so && cd $(SBCL_SRC) && ./make-shared-library.sh || true
	cp $(SBCL_SRC)/src/runtime/libsbcl.so ./

libcallback.core libcallback.c libcallback.h libcallback.py: 
	CL_SOURCE_REGISTRY="$(ROOT_DIR)//" $(SBCL_SRC)/run-sbcl.sh --script "script.lisp"

libcallback.dylib: libcallback.core libcallback.c
	$(CC) -dynamiclib -o $@ libcallback.c -L$(SBCL_SRC)/src/runtime -lsbcl
clean:
	rm -f libsbcl.so libcallback.c libcallback.h libcallback.core libcallback.py libcallback.dylib

