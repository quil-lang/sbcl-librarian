ROOT_DIR := $(shell dirname $(shell pwd))

.PHONY: all clean

all: libsbcl.so {{CLIBRARY}}.dylib

libsbcl.so: $(SBCL_SRC)
	test ! -e $(SBCL_SRC)/src/runtime/libsbcl.so && cd $(SBCL_SRC) && ./make-shared-library.sh || true
	cp $(SBCL_SRC)/src/runtime/libsbcl.so ./

{{CLIBRARY}}.core {{CLIBRARY}}.c {{CLIBRARY}}.h {{CLIBRARY}}.py:
	CL_SOURCE_REGISTRY="$(ROOT_DIR)//" $(SBCL_SRC)/run-sbcl.sh --load $(HOME)/quicklisp/setup.lisp --script "script.lisp"

{{CLIBRARY}}.dylib: {{CLIBRARY}}.core {{CLIBRARY}}.c
	$(CC) -dynamiclib -o $@ {{CLIBRARY}}.c -L$(SBCL_SRC)/src/runtime -lsbcl
clean:
	rm -f libsbcl.so {{CLIBRARY}}.c {{CLIBRARY}}.h {{CLIBRARY}}.core {{CLIBRARY}}.py {{CLIBRARY}}.dylib

