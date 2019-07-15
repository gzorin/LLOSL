cmake_minimum_required(VERSION 3.14)
project(LLOSL)

set(LLOSL_DEPS_DIR
  "${PROJECT_BINARY_DIR}/deps")

set(CMAKE_MODULE_PATH
  ${LLOSL_DEPS_DIR}/lib/cmake/Boost-1.70.0 ${CMAKE_MODULE_PATH})

include(ExternalProject)

# LLVM
find_package(LLVM REQUIRED CONFIG)
message(STATUS "Using LLVMConfig.cmake in: ${LLVM_DIR}")

# Boost
ExternalProject_Add(boost
  SOURCE_DIR ${PROJECT_SOURCE_DIR}/extsrc/boost
  CONFIGURE_COMMAND ${PROJECT_SOURCE_DIR}/extsrc/boost/bootstrap.sh
  BUILD_COMMAND ${PROJECT_SOURCE_DIR}/extsrc/boost/b2 -d0
  --build-dir=${PROJECT_BINARY_DIR}/extsrc/boost --prefix=${LLOSL_DEPS_DIR}
  --with-system --with-filesystem --with-regex --with-thread --with-wave --with-date_time
  variant=release link=static install
  BUILD_IN_SOURCE 1
  INSTALL_COMMAND ""
)
