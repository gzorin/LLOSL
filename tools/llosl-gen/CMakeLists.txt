llvm_map_components_to_libnames(LLVM_LIBRARIES core support bitreader bitwriter mcjit)

add_executable(llosl-gen
  llosl-gen.cpp)

target_compile_features(llosl-gen PUBLIC cxx_std_17)

target_include_directories(llosl-gen BEFORE
  PRIVATE
  ${CMAKE_SOURCE_DIR}/include)

target_include_directories(llosl-gen
  PUBLIC
  ${LLOSL_DEPS_DIR}/include
  ${LLVM_INCLUDE_DIRS}
  ${OPENEXR_INCLUDE_DIR})

target_link_libraries(llosl-gen
  LLOSL
  ${LLVM_LIBRARIES})