pushd lib
mkdir build
cd build
# Build libsbcl_librarian
cmake -DCMAKE_BUILD_TYPE=RelWithDebInfo ..
cmake --build .
cmake --install . --prefix=$PREFIX
popd

# Package Python library
cp lib/build/sbcl_librarian.py lib/python/src/sbcl_librarian/raw.py
$PYTHON -m pip install lib/python/ --no-deps --ignore-installed -vv
