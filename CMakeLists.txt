cmake_minimum_required(VERSION 3.14)
project(LLOSL LANGUAGES CXX)

option(LLOSL_SUPERBUILD "LLOSL superbuild" ON)

set(LLOSL_DEPS_DIR "${CMAKE_BINARY_DIR}/deps")

if (LLOSL_SUPERBUILD)
  include(${PROJECT_SOURCE_DIR}/SuperCMakeLists.txt)
  return()
endif()

# LLVM
find_package(LLVM REQUIRED CONFIG)
message(STATUS "Using LLVMConfig.cmake in: ${LLVM_DIR}")

find_package(OpenEXR REQUIRED)
find_package(OpenImageIO REQUIRED)

add_subdirectory(include)
add_subdirectory(lib)
