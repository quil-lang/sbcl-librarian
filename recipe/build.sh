if [[ "$OSTYPE" == "darwin"* && $(uname -m) == "arm64" ]]; then
    :
else
    export LD_LIBRARY_PATH=$CONDA_PREFIX/lib/sbcl # required because conda activation scripts are not run on install

    mkdir -p $PREFIX/etc/conda/activate.d
    mkdir -p $PREFIX/etc/conda/deactivate.d
    printf '#/bin/sh\n\nexport LD_LIBRARY_PATH=$CONDA_PREFIX/lib/sbcl\n' > $PREFIX/etc/conda/activate.d/env_vars.sh
    printf '#/bin/sh\n\nunset LD_LIBRARY_PATH' > $PREFIX/etc/conda/deactivate.d/env_vars.sh
fi

pushd lib
mkdir build
cd build
# Build libsbcl
cmake -DCMAKE_BUILD_TYPE=RelWithDebInfo ..
cmake --build .
cmake --install . --prefix=$PREFIX
popd

# Package Python library
cp lib/build/sbcl_librarian.py lib/python/src/sbcl_librarian/raw.py
$PYTHON -m pip install lib/python/ --no-deps --ignore-installed -vv
