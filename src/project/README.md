
## How to Use

Load the system and pass `make-project` the name of a system for which
you'd like sbcl-librarian bindings:

    (ql:quickload :sbcl-librarian/project)
    (sbcl-librarian.project:make-project :coolsystem)
    "New sbcl-librarian project in /path/top/coolsystem/lib/" 
    
    
This creates new project in `lib/` relative to the ASDF
system's directory. The new project is fairly skeletal - you are
expected to add bindings to your system's functions in the
`bindings.lisp` file.

The new project should look like this on your filesystem:

    lib/
    ├── Makefile
    ├── README
    ├── bindings.lisp
    ├── package.lisp
    ├── script.lisp
    └── libcoolsystem.asd
    
Add bindings to your library in `bindings.lisp`.

### The "template" Syntax

A new project is generated from a template directory. Files in this
directory are copied over to the target directory. Variables within
these files are capitalized, whitespace-free strings surrounded by
`{{` and `}}`.  E.g `{{SYSTEM}}`. Only a few variables are used

- `{{SYSTEM}}` the name of the system for which a shared library is being built.
- `{{LIBRARY}}` the name of the library being built. This name is reused in many places.
- `{{CLIBRARY}}` generated from LIBRARY, used wherever the C library is referenced. 
- `{{DESCRIPTION}}` a description of the library for the `ASDF` definition.
- `{{AUTHOR}}` author information for the `ASDF` defintion.


