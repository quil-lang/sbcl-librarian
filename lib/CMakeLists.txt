cmake_minimum_required(VERSION 3.12)

project(libsbcl_librarian C)

include(GNUInstallDirs)

set(CMAKE_INCLUDE_CURRENT_DIR ON)

add_library(sbcl_librarian SHARED libsbcl_librarian.c libsbcl_librarian.h libsbcl_librarian_err.h entry_point.c)
target_link_directories(sbcl_librarian PRIVATE $ENV{BUILD_PREFIX}/lib)
target_link_libraries(sbcl_librarian sbcl)

add_custom_command(
  OUTPUT libsbcl_librarian.c libsbcl_librarian.h
  COMMAND ${CMAKE_COMMAND} -E env CL_SOURCE_REGISTRY=${CMAKE_CURRENT_SOURCE_DIR}/..// sbcl --script ${CMAKE_CURRENT_SOURCE_DIR}/generate-bindings.lisp
  WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
)

install(TARGETS sbcl_librarian
  LIBRARY
  RUNTIME
)
install(FILES ${CMAKE_CURRENT_BINARY_DIR}/libsbcl_librarian.core TYPE LIB)
install(FILES ${CMAKE_CURRENT_BINARY_DIR}/libsbcl_librarian.h TYPE INCLUDE)
install(FILES ${CMAKE_CURRENT_SOURCE_DIR}/libsbcl_librarian_err.h TYPE INCLUDE)
