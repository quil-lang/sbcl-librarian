# sbcl-librarian
Opinionated interface for creating shared libraries. Requires SBCL
version >2.1.10.


## Building the example

In the `example/` directory there is a full example shown. It is
assumed sbcl-librarian is cloned in your local-projects directory so
it is visible to asdf. For example if you are using roswell:

```
$ cd ~/.roswell/local-projects/
$ git clone https://github.com/quil-lang/sbcl-librarian
```

the user needs to have the SBCL shared library built. To do so one
needs to run the `make-shared-libarary.sh` script. For roswell users
(at tha time of writing the lates SBCL version is 2.2.2):


```
$ cd ~/.roswell/src/sbcl-2.2.2/
$ ./make-shared-library.sh
```

Once that is done you are ready to build the example

```
$ make
$ ./example
> (+ 1 2)
3
```

(in case there is a conflict related to build IDs see
[issue](https://github.com/quil-lang/sbcl-librarian/issues/21#issuecomment-1068250667))

NOTE: On Mac OS X you *MUST* specify `-pagezero_size 0x100000` when
linking the final executable, otherwise SBCL will fail to mmap its
static space into the address `0x5000000`. This is because Mac decides
to make page zero take up a whole `4GB` (!), which prevents access to
the 32-bit address space. (Ostensibly to prevent null pointer or other
such bad (truncated?) pointer dereferences.)
