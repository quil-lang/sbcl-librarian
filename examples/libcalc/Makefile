EXAMPLES_DIR := $(shell pwd)
ROOT_DIR := $(shell dirname $(shell dirname $(EXAMPLES_DIR)))

ifeq ($(SBCL),)
    SBCL := sbcl
endif

libcalc_files := $(addprefix libcalc/,calc.c calc.h CMakeLists.txt      \
                                      fasl_loader.c incbin.h            \
                                      libcalc--system.fasl              \
                                      sbcl-librarian--system.fasl)
buildsystem_files := $(addprefix build/,Makefile CMakeCache.txt         \
                                        cmake_install.cmake             \
                                        libcalc--system.fasl            \
                                        sbcl-librarian--system.fasl)

ifeq ($(OS),Windows_NT)
    DYNAMIC_LIB_SUFFUX = dll
else
    UNAME_S := $(shell uname -s)
    ifeq ($(UNAME_S),Linux)
        DYNAMIC_LIB_SUFFUX = so
    endif
    ifeq ($(UNAME_S),Darwin)
        DYNAMIC_LIB_SUFFUX = dylib
    endif
endif

.PHONY: all clean

all: example

example: build/libcalc.$(DYNAMIC_LIB_SUFFUX)
	$(CC) example.c -o example -lsbcl_librarian -lcalc \
	      -L./build/ -I./libcalc/ $(CFLAGS)

$(libcalc_files) &: libcalc.asd libcalc.lisp bindings.lisp script.lisp
	CL_SOURCE_REGISTRY="$(ROOT_DIR)//" $(SBCL) --script "script.lisp"

$(buildsystem_files) &: $(libcalc_files)
	cmake -S libcalc/ -B build/

build/libcalc.$(DYNAMIC_LIB_SUFFUX): $(buildsystem_files)
	cmake --build build/

clean:
	rm -rf example libcalc/ build/
