# sbcl-librarian
Opinionated interface (based on ECL-SIMPLELIB) for creating shared
libraries. Requires SBCL version >2.1.9.

After loading this system, you can build the example library like this:

`sbcl --eval "(progn (ql:quickload :sbcl-librarian) (load \"example.lisp\"))"`

which produces a header file, a source file, and a core file, and then you can compile the artifacts like so with:

`gcc -c -fpic libcalc.c`

`gcc -shared libcalc.o -o libcalc.so`

`gcc example.c -o example -lsbcl -lcalc -L.`

which creates a shared library and executable using the functions
defined in the example system, assuming you have `libsbcl.so` and
`libcalc.so` in a shared library path somewhere.
