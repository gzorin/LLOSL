//-*-C++-*-
#ifndef LLOSLCONTEXTIMPL_H
#define LLOSLCONTEXTIMPL_H

#include "OSLErrorHandler.h"

#include <llosl/LLOSLContext.h>

#include <OSL/oslexec.h>

namespace llvm {
class LLVMContext;
} // End namespace llvm

namespace llosl {

class BuilderImpl;

class LLOSLContextImpl : private OSL::RendererServices {
public:

  inline static std::error_code make_error_code(LLOSLContext::Error e) {
      return std::error_code(static_cast<int>(e), LLOSLContext::ErrorCategory());
  }

  static const LLOSLContextImpl& Get(const LLOSLContext& context) { return *context.d_impl; }
  static LLOSLContextImpl&       Get(LLOSLContext& context)       { return *context.d_impl; }

  LLOSLContextImpl(llvm::LLVMContext&);
  ~LLOSLContextImpl();

  const llvm::LLVMContext&         getLLContext() const { return d_llcontext; }
  llvm::LLVMContext&               getLLContext()       { return d_llcontext; }

  const OSL::ShadingSystem&        getShadingSystem() const { return *d_shading_system.get(); }
  OSL::ShadingSystem&              getShadingSystem()       { return *d_shading_system.get(); }

  const OSL::ShadingContext       *getShadingContext() const { return d_shading_context; }
  OSL::ShadingContext             *getShadingContext()       { return d_shading_context; }

  OSLErrorScope                    enterOSLErrorScope();

  llvm::Expected<Builder>          getBuilder();
  void                             resetBuilder(BuilderImpl *);

  //
  using ShaderGroupListType = LLOSLContext::ShaderGroupListType;

  static ShaderGroupListType LLOSLContextImpl::*getSublistAccess(ShaderGroup *) {
      return &LLOSLContextImpl::d_shader_groups;
  }

  void                             addShaderGroup(ShaderGroup *);
  void                             removeShaderGroup(ShaderGroup *);

  //
  using BXDFScopeListType = LLOSLContext::BXDFScopeListType;

  static BXDFScopeListType LLOSLContextImpl::*getSublistAccess(BXDFScope *) {
      return &LLOSLContextImpl::d_bxdf_scopes;
  }

  void                             addBXDFScope(BXDFScope *);
  void                             removeBXDFScope(BXDFScope *);

private:

  void registerClosures();

  // OSL::RendererServices overrides:
  llvm::LLVMContext *llvm_context() const override;
  int supports(OSL::string_view) const override;

  OSLErrorHandler d_osl_error_handler;

  llvm::LLVMContext& d_llcontext;
  std::unique_ptr<OSL::ShadingSystem> d_shading_system;
  OSL::ShadingContext *d_shading_context;

  BuilderImpl *d_builder = nullptr;

  ShaderGroupListType d_shader_groups;
  BXDFScopeListType d_bxdf_scopes;
};

}

#endif
