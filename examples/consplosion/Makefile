EXAMPLES_DIR := $(shell pwd)
ROOT_DIR := $(shell dirname $(shell dirname $(EXAMPLES_DIR)))


.PHONY: all clean

all: libsbcl.so consplosion.dylib

# For some reason, even on OSX SBCL builds its library with *.so
# extension, but this works!
libsbcl.so: $(SBCL_SRC)
	test ! -e $(SBCL_SRC)/src/runtime/libsbcl.so && cd $(SBCL_SRC) && ./make-shared-library.sh || true
	cp $(SBCL_SRC)/src/runtime/libsbcl.so ./

consplosion.core consplosion.c consplosion.h consplosion.py: consplosion.lisp
	CL_SOURCE_REGISTRY="$(ROOT_DIR)//" $(SBCL_SRC)/run-sbcl.sh --script "script.lisp"

consplosion.dylib: consplosion.core consplosion.c
	$(CC) -dynamiclib -o $@ consplosion.c -L$(SBCL_SRC)/src/runtime -lsbcl
clean:
	rm -f libsbcl.so consplosion.c consplosion.h consplosion.core consplosion.py consplosion.dylib
