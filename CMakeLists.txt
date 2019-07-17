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
find_package(tiff REQUIRED)
find_package(png REQUIRED)
find_package(jpeg REQUIRED)
find_package(OpenImageIO REQUIRED)
find_package(Boost 1.70.0 REQUIRED COMPONENTS filesystem regex thread wave date_time system)

find_library(OpenShadingLanguage_exec_LIBRARY
  NAMES oslexec
  PATHS ${LLOSL_DEPS_DIR}/lib
  NO_DEFAULT_PATH)

find_library(OpenShadingLanguage_query_LIBRARY
  NAMES oslquery
  PATHS ${LLOSL_DEPS_DIR}/lib
  NO_DEFAULT_PATH)

find_library(OpenShadingLanguage_comp_LIBRARY
  NAMES oslcomp
  PATHS ${LLOSL_DEPS_DIR}/lib
  NO_DEFAULT_PATH)

set(OpenShadingLanguage_LIBRARIES 
  ${OpenShadingLanguage_exec_LIBRARY} ${OpenShadingLanguage_query_LIBRARY} ${OpenShadingLanguage_comp_LIBRARY})

install(
  EXPORT LLOSLTargets
  DESTINATION lib/cmake/llosl)

add_subdirectory(include)
add_subdirectory(lib)

include(CMakePackageConfigHelpers)

configure_package_config_file(
  ${CMAKE_CURRENT_SOURCE_DIR}/cmake/LLOSLConfig.cmake.in ${CMAKE_CURRENT_BINARY_DIR}/LLOSLConfig.cmake
  INSTALL_DESTINATION lib/cmake/llosl)

install(
  FILES ${CMAKE_CURRENT_BINARY_DIR}/LLOSLConfig.cmake
  DESTINATION lib/cmake/llosl)
