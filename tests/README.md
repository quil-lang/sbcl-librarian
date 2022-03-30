# Testing sbcl-librarian

## Requirements
- [pytest](docs.pytest.org)

**NOTE**: the test suite currently only runs on MacOS.

## How do I...
### Add a test library
Add a new directory to `libs/` with the same name as the library (e.g. `libsquare`). The Makefile that builds the test libraries only requires each library directory to have a `script.lisp` file that uses `sbcl-librarian` to generate and dump bindings for the library, but you probably want to add additional files such as package and system definitions. Use `libsquare/` as a model.

The test suite automatically detects new directories in `lib/` and makes sure they get built whenever the suite is run.

### Add Python tests
Add `test_*.py` files to `python/`. The test suite just shells out to `pytest` in the `python/` directory, so see [the pytest documentation](docs.pytest.org) for details on how to write tests.

The test suite ensures that the Python tests are run in an environment where the test libraries are available for import.
