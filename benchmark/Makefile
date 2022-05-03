.PHONY: all clean

all: benchmark.dylib benchmark_c.dylib

benchmark.core benchmark.c benchmark.h benchmark.py: benchmark.lisp
	$(SBCL_SRC)/run-sbcl.sh --script "script.lisp"

benchmark.dylib: benchmark.core benchmark.c
	$(CC) -dynamiclib -o $@ benchmark.c -L$(SBCL_SRC)/src/runtime -lsbcl
benchmark_c.dylib: benchmark_c.c
	$(CC) -dynamiclib -o $@ benchmark_c.c
clean:
	rm -f benchmark.c benchmark.h benchmark.core benchmark.py benchmark.dylib benchmark_c.dylib
