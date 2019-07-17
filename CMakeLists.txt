cmake_minimum_required(VERSION 3.14)
project(LLOSL LANGUAGES CXX)

option(LLOSL_SUPERBUILD "LLOSL superbuild" ON)

set(LLOSL_DEPS_DIR
  ${PROJECT_BINARY_DIR}/deps)

list(APPEND CMAKE_MODULE_PATH
  ${LLOSL_DEPS_DIR}/share/cmake/Modules)

if (LLOSL_SUPERBUILD)
  include(${PROJECT_SOURCE_DIR}/SuperCMakeLists.txt)
  return()
endif()

list(APPEND CMAKE_MODULE_PATH
  ${PROJECT_SOURCE_DIR}/extsrc/OpenShadingLanguage/src/cmake/modules)

# LLVM
find_package(LLVM REQUIRED CONFIG)
message(STATUS "Using LLVMConfig.cmake in: ${LLVM_DIR}")

find_package(OpenEXR REQUIRED)

set(OPENIMAGEIO_ROOT_DIR ${LLOSL_DEPS_DIR})
find_package(OpenImageIO REQUIRED)

add_subdirectory(include)
add_subdirectory(lib)
