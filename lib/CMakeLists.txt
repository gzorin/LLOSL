add_definitions(${LLVM_DEFINITIONS})

add_library(LLOSL STATIC
  LLOSLContext.cpp)

target_compile_features(LLOSL PUBLIC cxx_std_17)

target_include_directories(LLOSL
  PUBLIC
  ${LLVM_INCLUDE_DIRS}
  ${OPENEXR_INCLUDE_DIR}
  PRIVATE
  ${PROJECT_SOURCE_DIR}/include
  ${LLOSL_DEPS_DIR}/include)

llvm_map_components_to_libnames(LLVM_LIBRARIES core support bitreader bitwriter mcjit)

target_link_libraries(LLOSL
  ${LLVM_LIBRARIES}
  ${OpenShadingLanguage_LIBRARIES} ${OPENIMAGEIO_LIBRARIES}
  ${OPENEXR_LIBRARIES} ${TIFF_LIBRARIES} ${PNG_LIBRARIES} ${JPEG_LIBRARIES}
  ${Boost_LIBRARIES})

install(
  TARGETS LLOSL
  EXPORT LLOSLTargets
  ARCHIVE
  DESTINATION lib)
