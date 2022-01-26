# sbcl-librarian
Opinionated interface for creating shared libraries. Requires SBCL
version >2.1.10.

After loading this system, you can build the example library like this:

`sbcl --eval "(progn (asdf:load-system :sbcl-librarian) (load \"libcalc.lisp\"))"`

which produces a header file, a source file, and a core file, and then you can compile the artifacts like so with:

`gcc -c -fpic libcalc.c`

`gcc -shared libcalc.o -o libcalc.so -lsbcl`

`gcc example.c -o example -lsbcl -lcalc -L.`

which creates a shared library and executable using the functions
defined in the example system, assuming you have `libsbcl.so` and
`libcalc.so` in a shared library path somewhere.

NOTE: On Mac OS X you *MUST* specify `-pagezero-size 0x100000` when
linking the final executable, otherwise SBCL will fail to mmap its
static space into the address `0x5000000`. This is because Mac decides
to make page zero take up a whole `4GB` (!), which prevents access to
the 32-bit address space. (Ostensibly to prevent null pointer or other
such bad (truncated?) pointer dereferences.)
