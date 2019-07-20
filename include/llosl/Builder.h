//-*-C++-*-
#ifndef LLOSL_BUILDER_H
#define LLOSL_BUILDER_H

#include <llvm/ADT/StringRef.h>

namespace llosl {

class LLOSLContextImpl;

class Builder {
public:

  Builder() = delete;
  Builder(const Builder&) = delete;
  Builder(Builder&&);
  ~Builder();

  void BeginShaderGroup(llvm::StringRef, llvm::StringRef);
  void EndShaderGroup();

private:

  friend class LLOSLContextImpl;

  Builder(LLOSLContextImpl& context);

  LLOSLContextImpl *reset();

  LLOSLContextImpl *d_context;
};

} // End namespace llosl

#endif
