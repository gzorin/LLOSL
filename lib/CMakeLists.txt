add_definitions(${LLVM_DEFINITIONS})

add_library(LLOSL STATIC
  LLOSLContext.cpp)

target_compile_features(LLOSL PUBLIC cxx_std_17)

target_include_directories(LLOSL
  PUBLIC
  ${LLVM_INCLUDE_DIRS}
  ${OPENIMAGEIO_INCLUDE_DIR}
  ${OPENEXR_INCLUDE_DIR}
  ${ILMBASE_INCLUDE_DIR}
  ${ILMBASE_INCLUDE_DIR}/OpenEXR
  PRIVATE
  ${PROJECT_SOURCE_DIR}/include
  ${LLOSL_DEPS_DIR}/include)

install(
  TARGETS LLOSL EXPORT LLOSL
  ARCHIVE
  DESTINATION lib
)
