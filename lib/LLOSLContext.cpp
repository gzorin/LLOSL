#include <llosl/LLOSLContext.h>

#include <llvm/IR/LLVMContext.h>

#include "LLOSLContextImpl.h"

#include <map>

namespace llosl {

// Implementation:
LLOSLContextImpl::LLOSLContextImpl(llvm::LLVMContext& llcontext)
  : d_llcontext(llcontext)
  , d_shading_system(new OSL::ShadingSystem(this))
  , d_shading_context(d_shading_system->get_context(d_shading_system->create_thread_info())) {
  d_shading_system->attribute("lockgeom", 0);
  d_shading_system->attribute("optimize", 0);
}

LLOSLContextImpl::~LLOSLContextImpl() {
}

llvm::LLVMContext *
LLOSLContextImpl::llvm_context() const {
  return &d_llcontext;
}

// Interface:
namespace {
namespace contexts {

std::map<llvm::LLVMContext*, LLOSLContext *>&
llvm_to_llosl() {
  static std::map<llvm::LLVMContext*, LLOSLContext *> s_llvm_to_llosl;
  return s_llvm_to_llosl;
}

} // End namespace contexts
} // End anonymous namespace

const LLOSLContext*
LLOSLContext::Get(const llvm::LLVMContext* llcontext) {
  auto it = contexts::llvm_to_llosl().find(const_cast<llvm::LLVMContext *>(llcontext));
  return it != contexts::llvm_to_llosl().end()?
    it->second :
    nullptr;
}

LLOSLContext*
LLOSLContext::Get(llvm::LLVMContext* llcontext) {
  auto it = contexts::llvm_to_llosl().find(llcontext);
  return it != contexts::llvm_to_llosl().end()?
    it->second :
    nullptr;
}

LLOSLContext::LLOSLContext(llvm::LLVMContext& llcontext)
  : d_impl(new LLOSLContextImpl(llcontext)) {
  contexts::llvm_to_llosl().insert(std::make_pair(&llcontext, this));
}

LLOSLContext::~LLOSLContext() {
  contexts::llvm_to_llosl().erase(&d_impl->getLLContext());
}

const llvm::LLVMContext&
LLOSLContext::getLLContext() const {
  return d_impl->getLLContext();
}

llvm::LLVMContext&
LLOSLContext::getLLContext() {
  return d_impl->getLLContext();
}

} // End namespace llosl
