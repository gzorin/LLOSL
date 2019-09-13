add_definitions(${LLVM_DEFINITIONS})

add_library(LLOSLIR STATIC
  Block.cpp
  ClosureFunction.cpp
  ClosureIRPass.cpp
  Instruction.cpp
  Use.cpp
  User.cpp
  Value.cpp)

target_compile_features(LLOSLIR PUBLIC cxx_std_17)

target_include_directories(LLOSLIR BEFORE
  PRIVATE
  ${CMAKE_SOURCE_DIR}/include
  ${LLOSL_DEPS_DIR}/include)

target_include_directories(LLOSLIR
  PUBLIC
  ${LLVM_INCLUDE_DIRS}
  ${OPENEXR_INCLUDE_DIR})

llvm_map_components_to_libnames(LLVM_LIBRARIES core support)

target_link_libraries(LLOSLIR
  ${LLVM_LIBRARIES}
  ${Boost_LIBRARIES})

install(
  TARGETS LLOSLIR
  EXPORT LLOSLTargets
  ARCHIVE
  DESTINATION lib)
