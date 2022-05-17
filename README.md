# sbcl-librarian
Opinionated interface for creating shared libraries. Requires SBCL
version >2.1.10.

You can build the example library using the Makefile in the example
directory, which produces a header file, a source file, and a core
file

You can compile example like so with:

```
export SBCL_SRC=~/.roswell/src/sbcl-2.2.4

cd example
make

./example
```

which creates a shared library and executable using the functions
defined in the example system, assuming you have `libsbcl.so`
in a `${SBCL_SRC}/src/runtime/`.

If you don't have `libsbcl.so`, make will try to build it for you
by calling `make-shared-library.sh` at `${SBCL_SRC}` directory.

NOTE: On Intel Mac OS X you *MUST* specify `-pagezero_size 0x100000`
when linking the final executable, otherwise SBCL will fail to mmap
its static space into the address `0x5000000`. This is because Mac
decides to make page zero take up a whole `4GB` (!), which prevents
access to the 32-bit address space. (Ostensibly to prevent null
pointer or other such bad (truncated?) pointer dereferences.)

# Runtime notes
The SBCL runtime includes a garbage collector and memory allocator.

Process resources SBCL uses:
- Signal handlers into the process for things like keyboard interrupts
  (can be turned off). On some platforms this may also entail a
  stop-the-world GC signal.
- File descriptors like standard input and output.
- A fixed address space somewhere in memory. On x86 (32-bit and
  64-bit) this needs to be a fixed address space under 4 GB. This is
  so the compiler can generate manifest literal addresses in machine
  code on some platforms, as well as to compute stable hashes of
  certain constants like NIL.
