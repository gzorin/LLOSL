cmake_minimum_required(VERSION 3.14)
project(LLOSL)

# LLVM
find_package(LLVM REQUIRED CONFIG)
message(STATUS "Using LLVMConfig.cmake in: ${LLVM_DIR}")

