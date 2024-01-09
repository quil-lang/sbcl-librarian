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
  OUTPUT_VARIABLE libsbclLinkCommand
)
message("${libsbclLinkCommand}")
string(REGEX MATCHALL "[^ \t\r\n]+\.pic\.o" sbclRuntimePicObjectFiles "${libsbclLinkCommand}")
list(TRANSFORM sbclRuntimePicObjectFiles PREPEND "${sbcl_SOURCE_DIR}/src/runtime/")
message("${sbclRuntimePicObjectFiles}")

set_source_files_properties(
  sbclRuntimePicObjectFiles
  PROPERTIES
  EXTERNAL_OBJECT TRUE
  GENERATED TRUE
)

add_library(sbcl SHARED libsbcl.cpp ${sbclRuntimePicObjectFiles})
