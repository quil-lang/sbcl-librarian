pushd lib
mkdir build
pushd build
# Build libsbcl_librarian
& $env:MSYS2_CMD -c 'cmake -DCMAKE_PREFIX_PATH=$BUILD_PREFIX -DCMAKE_INSTALL_PREFIX=$PREFIX -DCMAKE_BUILD_TYPE=RelWithDebInfo -G ''MSYS Makefiles'' ..'
& $env:MSYS2_CMD -c "cmake --build ."
& $env:MSYS2_CMD -c "cmake --install ."
popd
popd

# Package Python library
cp lib/build/sbcl_librarian.py lib/python/src/sbcl_librarian/raw.py
& $env:PYTHON -m pip install lib/python/ --no-deps --ignore-installed -vv
