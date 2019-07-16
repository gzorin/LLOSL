//-*-C++-*-
#ifndef LLOSLCONTEXTIMPL_H
#define LLOSLCONTEXTIMPL_H

#include <llosl/LLOSLContext.h>

#include <OSL/oslexec.h>

namespace llvm {
class LLVMContext;
} // End namespace llvm

namespace llosl {

class LLOSLContextImpl : private OSL::RendererServices {
public:
  
  static const LLOSLContextImpl& Get(const LLOSLContext& context) { return *context.d_impl; }
  static LLOSLContextImpl&       Get(LLOSLContext& context)       { return *context.d_impl; }

  LLOSLContextImpl(llvm::LLVMContext&);
  ~LLOSLContextImpl();

  const llvm::LLVMContext&         getLLContext() const { return d_llcontext; }
  llvm::LLVMContext&               getLLContext()       { return d_llcontext; }

private:

  // OSL::RendererServices overrides:
  llvm::LLVMContext *llvm_context() const override;

  llvm::LLVMContext& d_llcontext;
  std::unique_ptr<OSL::ShadingSystem> d_shading_system;
  OSL::ShadingContext *d_shading_context;
};

}

#endif
