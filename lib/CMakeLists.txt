cmake_minimum_required(VERSION 3.12)

project(libsbcl_librarian C)

include(GNUInstallDirs)

set(CMAKE_INCLUDE_CURRENT_DIR ON)

add_library(sbcl_librarian SHARED sbcl_librarian.c sbcl_librarian.h sbcl_librarian_err.h entry_point.c)
if(WIN32)
# DLLs go to the bin/ directory on Windows
target_link_directories(sbcl_librarian PRIVATE $ENV{BUILD_PREFIX}/Library/bin)
# SBCL names its DLL `libsbcl.so', so it can not be found the normal way.
find_file(SBCL_LIBRARY libsbcl.so PATH_SUFFIXES lib)
else()
target_link_directories(sbcl_librarian PRIVATE $ENV{BUILD_PREFIX}/lib)
find_library(SBCL_LIBRARY sbcl)
endif()
target_link_libraries(sbcl_librarian PUBLIC ${SBCL_LIBRARY})

# Drop the lib prefix on the Windows DLL
if(WIN32)
  set_target_properties(sbcl_librarian PROPERTIES PREFIX "")
endif(WIN32)

if(DEFINED ENV{SBCL})
    set(SBCL "$ENV{SBCL}")
elseif(DEFINED ENV{SBCL_SRC})
    set(SBCL sh "$ENV{SBCL_SRC}/run-sbcl.sh")
else()
    set(SBCL "sbcl")
endif()

# Platform-specific path separator
if(WIN32)
  set(SBCL_ENV_PATH_SEPARATOR "\\\;")
else()
  set(SBCL_ENV_PATH_SEPARATOR ":")
endif()


# Generate the core file and C bindings
add_custom_command(
  OUTPUT sbcl_librarian.c sbcl_librarian.h
  COMMAND ${CMAKE_COMMAND} -E env CL_SOURCE_REGISTRY=${CMAKE_CURRENT_SOURCE_DIR}/..//${SBCL_ENV_PATH_SEPARATOR} ${SBCL} --script ${CMAKE_CURRENT_SOURCE_DIR}/generate-bindings.lisp
  WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
)

# DLLs go to the bin/ directory on Windows
if(WIN32)
  install(TARGETS sbcl_librarian RUNTIME)
  install(FILES ${CMAKE_CURRENT_BINARY_DIR}/sbcl_librarian.core TYPE BIN)
else()
  install(TARGETS sbcl_librarian LIBRARY)
  install(FILES ${CMAKE_CURRENT_BINARY_DIR}/sbcl_librarian.core TYPE LIB)
endif(WIN32)

install(FILES ${CMAKE_CURRENT_BINARY_DIR}/sbcl_librarian.h TYPE INCLUDE)
install(FILES ${CMAKE_CURRENT_SOURCE_DIR}/sbcl_librarian_err.h TYPE INCLUDE)
