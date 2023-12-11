EXAMPLES_DIR := $(shell pwd)
ROOT_DIR := $(shell dirname $(shell dirname $(EXAMPLES_DIR)))

.PHONY: all clean

OS:=$(shell uname -s)
ifeq ($(OS), Darwin)
	CALC_SO_NAME = libcalc.dylib
	CCFLAGS = -dynamiclib
else
	CALC_SO_NAME = libcalc.so
	CCFLAGS = -shared
endif

all: example

example: libsbcl.so $(CALC_SO_NAME)
	LD_LIBRARY_PATH=. $(CC) example.c -o example -lsbcl -lcalc -L.

# For some reason, even on OSX SBCL builds its library with *.so
# extension, but this works!
libsbcl.so: $(SBCL_SRC)
	test ! -e $(SBCL_SRC)/src/runtime/libsbcl.so && cd $(SBCL_SRC) && ./make-shared-library.sh || true
	cp $(SBCL_SRC)/src/runtime/libsbcl.so ./

libcalc.core libcalc.c libcalc.h libcalc.py: libcalc.lisp
	CL_SOURCE_REGISTRY="$(ROOT_DIR)//" $(SBCL_SRC)/run-sbcl.sh --script "script.lisp"

$(CALC_SO_NAME): libcalc.core libcalc.c
	$(CC) $(CCFLAGS) -o $@ libcalc.c -L$(SBCL_SRC)/src/runtime -lsbcl

clean:
	rm -f example libsbcl.so libcalc.c libcalc.h libcalc.core libcalc.py libcalc.dylib
