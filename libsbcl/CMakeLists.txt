cmake_minimum_required(VERSION 3.12)

project(libsbcl)

include(FetchContent)
FetchContent_Declare(
  sbcl
  GIT_REPOSITORY git://git.code.sf.net/p/sbcl/sbcl
)
FetchContent_MakeAvailable(sbcl)

execute_process(
  COMMAND sh make.sh
  WORKING_DIRECTORY ${sbcl_SOURCE_DIR}
)
execute_process(
  COMMAND sh make-shared-library.sh
  WORKING_DIRECTORY ${sbcl_SOURCE_DIR}
)
execute_process(
  COMMAND make -B -n -C src/runtime/ libsbcl.so
  COMMAND tail -n 1
  WORKING_DIRECTORY ${sbcl_SOURCE_DIR}
  OUTPUT_VARIABLE runtimeLinkCommand
)
string(REGEX MATCHALL "[^ \t\r\n]+\.pic\.o" runtimePicObjectFiles "${runtimeLinkCommand}")
list(TRANSFORM runtimePicObjectFiles PREPEND "${sbcl_SOURCE_DIR}/src/runtime/")

set_source_files_properties(
  runtimePicObjectFiles
  PROPERTIES
  EXTERNAL_OBJECT TRUE
  GENERATED TRUE
)

add_library(sbcl SHARED libsbcl.c ${runtimePicObjectFiles})

add_custom_target(
  libsbcl ALL
  COMMAND sh ${sbcl_SOURCE_DIR}/run-sbcl.sh --core ${sbcl_SOURCE_DIR}/output/sbcl.core --script ${CMAKE_CURRENT_SOURCE_DIR}/embed-core.lisp $<TARGET_FILE:sbcl> ${CMAKE_SHARED_LIBRARY_SUFFIX}
  DEPENDS sbcl
)

install(TARGETS sbcl LIBRARY RUNTIME)
install(FILES libsbcl.h TYPE INCLUDE)
