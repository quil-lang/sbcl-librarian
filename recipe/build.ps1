# Build sbcl
$env:CHERE_INVOKING="yes"
$env:MSYS2_PATH_TYPE="inherit"
$env:MSYSTEM="MINGW64"
C:\msys64\usr\bin\bash.exe -lc "cmake -G 'MSYS Makefiles' -DCMAKE_BUILD_TYPE=RelWithDebInfo .."

# Install SBCL into Conda prefix
pushd ../sbcl
robocopy .\output $env:LIBRARY_BIN sbcl.core
robocopy .\src\runtime $env:LIBRARY_BIN sbcl.exe
robocopy .\obj\sbcl-home\contrib $env:LIBRARY_BIN\contrib /e
robocopy .\src\runtime $env:LIBRARY_LIB libsbcl.a
popd

# Build libsbcl
C:\msys64\usr\bin\bash.exe -lc 'cmake --build .'
C:\msys64\usr\bin\bash.exe -lc "cmake --install . --prefix=$env:PREFIX"
popd

# Un/set SBCL_HOME on environment de/activation
mkdir -p $env:CONDA_PREFIX\etc\conda\activate.d
mkdir -p $env:CONDA_PREFIX\etc\conda\deactivate.d
"set SBCL_HOME=%CONDA_PREFIX%\Library\bin" | Out-File $env:CONDA_PREFIX\etc\conda\activate.d\env_vars.bat
"set SBCL_HOME=" | Out-File $env:CONDA_PREFIX\etc\conda\deactivate.d\env_vars.bat
