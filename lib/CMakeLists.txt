add_definitions(${LLVM_DEFINITIONS})

add_library(LLOSL STATIC
  Builder.cpp
  BXDF.cpp
  LLOSLContext.cpp
  OSLErrorHandler.cpp
  Shader.cpp
  ShaderGroup.cpp
  UberBXDF.cpp)

target_compile_features(LLOSL PUBLIC cxx_std_17)

target_include_directories(LLOSL BEFORE
  PRIVATE
  ${CMAKE_SOURCE_DIR}/include
  ${LLOSL_DEPS_DIR}/include
  ${CMAKE_SOURCE_DIR}/extsrc/OpenShadingLanguage/src/include
  ${CMAKE_SOURCE_DIR}/extsrc/OpenShadingLanguage/src/liboslexec)

target_include_directories(LLOSL
  PUBLIC
  ${LLVM_INCLUDE_DIRS}
  ${OPENEXR_INCLUDE_DIR})

llvm_map_components_to_libnames(LLVM_LIBRARIES core support bitreader bitwriter mcjit transformutils passes)

target_link_libraries(LLOSL
  LLOSLIR
  ${LLVM_LIBRARIES}
  ${OpenShadingLanguage_LIBRARIES} ${OPENIMAGEIO_LIBRARIES}
  ${OPENEXR_LIBRARIES} ${TIFF_LIBRARIES} ${PNG_LIBRARIES} ${JPEG_LIBRARIES}
  ${Boost_LIBRARIES})

install(
  TARGETS LLOSL
  EXPORT LLOSLTargets
  ARCHIVE
  DESTINATION lib)
