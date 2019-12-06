//-*-C++-*-
#ifndef LLOSLCONTEXTIMPL_H
#define LLOSLCONTEXTIMPL_H

#include "OSLErrorHandler.h"

#include <llosl/LLOSLContext.h>
#include <llosl/BXDF.h>
#include <llosl/UberBXDF.h>
#include <llosl/IR/BXDFAST.h>

#include <OSL/oslexec.h>

#include <osl_pvt.h>
#include <oslexec_pvt.h>

#include <map>
#include <tuple>
#include <unordered_map>

namespace std {
    template<>
    struct hash<OSL::pvt::ShaderMaster::ref> {
        std::size_t operator()(OSL::pvt::ShaderMaster::ref p) const;
    };
}

namespace llvm {
class FunctionType;
class LLVMContext;
class Module;
} // End namespace llvm

namespace llosl {

class BuilderImpl;
class Shader;

class LLOSLContextImpl : private OSL::RendererServices {
public:

  inline static std::error_code make_error_code(LLOSLContext::Error e) {
      return std::error_code(static_cast<int>(e), LLOSLContext::ErrorCategory());
  }

  static const LLOSLContextImpl& Get(const LLOSLContext& context) { return *context.d_impl; }
  static LLOSLContextImpl&       Get(LLOSLContext& context)       { return *context.d_impl; }

  LLOSLContextImpl(llvm::LLVMContext&, unsigned = 0);
  ~LLOSLContextImpl();

  const llvm::LLVMContext&         getLLContext() const { return d_llcontext; }
  llvm::LLVMContext&               getLLContext()       { return d_llcontext; }

  llvm::Type                      *getLLVMType(const OSL::TypeDesc&, bool);
  llvm::Constant                  *getLLVMDefaultConstant(const OSL::TypeDesc&, bool);
  std::pair<llvm::Constant *, const void *> getLLVMConstant(const OSL::TypeDesc&, const void *, bool);

  bool                             isTypePassedByReference(const OSL::TypeDesc&) const;
  llvm::Type                      *getLLVMTypeForArgument(const OSL::TypeDesc&, bool);

  const OSL::ShadingSystem&        getShadingSystem() const { return *d_shading_system.get(); }
  OSL::ShadingSystem&              getShadingSystem()       { return *d_shading_system.get(); }

  const OSL::ShadingContext       *getShadingContext() const { return d_shading_context; }
  OSL::ShadingContext             *getShadingContext()       { return d_shading_context; }

  unsigned                         bxdf_address_space() const { return d_bxdf_address_space; }

  std::tuple<const BXDF *, unsigned, bool> getOrInsertBXDF(BXDF::EncodingView, BXDFAST::NodeRef, std::size_t);

  llvm::Module                    *bxdf_module()       { return d_bxdf_module.get(); }
  const llvm::Module              *bxdf_module() const { return d_bxdf_module.get(); }

  using BXDFComponentMapType = std::map<unsigned, llvm::Function *>;

  BXDFComponentMapType&            bxdf_components()       { return d_bxdf_components; }
  const BXDFComponentMapType&      bxdf_components() const { return d_bxdf_components; }

  llvm::Function                  *getBXDFComponent(unsigned id) const;

  UberBXDF                        *uber_bxdf() const   { return d_uber_bxdf; }

  //
  OSLErrorScope                    enterOSLErrorScope();

  llvm::Expected<Builder>          getBuilder();
  void                             resetBuilder(BuilderImpl *);

  //
  llvm::Expected<Shader *>         createShaderFromFile(llvm::StringRef);

  llvm::Expected<Shader *>         getShaderFromShaderMaster(OSL::pvt::ShaderMaster::ref);

  //
  using ShaderListType = LLOSLContext::ShaderListType;

  static ShaderListType LLOSLContextImpl::*getSublistAccess(Shader *) {
      return &LLOSLContextImpl::d_shaders;
  }

  void                             addShader(Shader *);
  void                             removeShader(Shader *);

  //
  using BXDFListType = LLOSLContext::BXDFListType;

  static BXDFListType LLOSLContextImpl::*getSublistAccess(BXDF *) {
      return &LLOSLContextImpl::d_bxdfs;
  }

  void                             addBXDF(BXDF *);
  void                             removeBXDF(BXDF *);

  //
  using UberBXDFListType = LLOSLContext::UberBXDFListType;

  static UberBXDFListType LLOSLContextImpl::*getSublistAccess(UberBXDF *) {
      return &LLOSLContextImpl::d_uber_bxdfs;
  }

  void                             addUberBXDF(UberBXDF *);
  void                             removeUberBXDF(UberBXDF *);

  //
  using ShaderGroupListType = LLOSLContext::ShaderGroupListType;

  static ShaderGroupListType LLOSLContextImpl::*getSublistAccess(ShaderGroup *) {
      return &LLOSLContextImpl::d_shader_groups;
  }

  void                             addShaderGroup(ShaderGroup *);
  void                             removeShaderGroup(ShaderGroup *);

private:

  void registerClosures();

  // OSL::RendererServices overrides:
#if 0
  llvm::LLVMContext *llvm_context() const override;
#endif
  int supports(OSL::string_view) const override;

  OSLErrorHandler d_osl_error_handler;

  llvm::LLVMContext& d_llcontext;

  llvm::Type *d_string_type = nullptr;

  std::unique_ptr<llvm::Module> d_bxdf_module;
  BXDFComponentMapType d_bxdf_components;

  std::unique_ptr<OSL::ShadingSystem> d_shading_system;
  OSL::ShadingContext *d_shading_context;

  BuilderImpl *d_builder = nullptr;

  ShaderListType d_shaders;

  std::unordered_map<OSL::pvt::ShaderMaster::ref, Shader *> d_shader_masters;

  unsigned d_bxdf_address_space = 0;
  BXDFListType d_bxdfs;
  UberBXDFListType d_uber_bxdfs;
  ShaderGroupListType d_shader_groups;

  UberBXDF *d_uber_bxdf = nullptr;

  std::map<BXDF::Encoding, const BXDF *, std::less<> > d_bxdf_index;
};

}

#endif
