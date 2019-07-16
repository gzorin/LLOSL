cmake_minimum_required(VERSION 3.14)
project(LLOSL)

option(LLOSL_SUPERBUILD "LLOSL superbuild" ON)

set(LLOSL_DEPS_DIR
  ${PROJECT_BINARY_DIR}/deps)

list(APPEND CMAKE_MODULE_PATH
  ${LLOSL_DEPS_DIR}/lib/cmake/Boost-1.70.0
  ${LLOSL_DEPS_DIR}/share/cmake/Modules)

# LLVM
find_package(LLVM REQUIRED CONFIG)
message(STATUS "Using LLVMConfig.cmake in: ${LLVM_DIR}")

if (LLOSL_SUPERBUILD)
  include(${PROJECT_SOURCE_DIR}/CMakeListsSuper.cmake)
  return()
endif()
