//-*-C++-*-
#ifndef LLOSL_LLOSLCONTEXT_H
#define LLOSL_LLOSLCONTEXT_H

#include <memory>

namespace llvm {
class LLVMContext;
} // End namespace llvm

namespace llosl {

class LLOSLContextImpl;

class LLOSLContext {
public:

  static LLOSLContext       *Get(llvm::LLVMContext *);
  static const LLOSLContext *Get(const llvm::LLVMContext *);

  LLOSLContext(llvm::LLVMContext&);
  ~LLOSLContext();

  const llvm::LLVMContext&   getLLContext() const;
  llvm::LLVMContext&         getLLContext();

private:

  std::unique_ptr<LLOSLContextImpl> d_impl;
  
  friend class LLOSLContextImpl;
};

} // End namespace llosl

#endif
