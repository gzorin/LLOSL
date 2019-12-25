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
#include <memory>
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
class PointerType;
class StructType;
} // End namespace llvm

namespace llosl {

class BuilderImpl;
class Closure;
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

  //
  llvm::Type                      *getLLVMType(const OIIO::TypeDesc&, bool);
  llvm::Constant                  *getLLVMDefaultConstant(const OIIO::TypeDesc&, bool);
  std::pair<llvm::Constant *, const void *> getLLVMConstant(const OIIO::TypeDesc&, const void *, bool);

  bool                             isTypePassedByReference(const OIIO::TypeDesc&) const;
  llvm::Type                      *getLLVMTypeForArgument(const OIIO::TypeDesc&, bool);

  llvm::StructType                *getLLVMStringType();

  llvm::StructType                *getLLVMClosureType();
  llvm::Constant                  *getLLVMClosureDefaultConstant();

  llvm::PointerType               *getLLVMClosurePointerType();
  llvm::Constant                  *getLLVMClosurePointerDefaultConstant();

  llvm::StructType                *getShaderGlobalsType();

  //
  const OSL::ShadingSystem&        getShadingSystem() const { return *d_shading_system.get(); }
  OSL::ShadingSystem&              getShadingSystem()       { return *d_shading_system.get(); }

  const OSL::ShadingContext       *getShadingContext() const { return d_shading_context; }
  OSL::ShadingContext             *getShadingContext()       { return d_shading_context; }

  //
  using ClosureMapType = llvm::DenseMap<unsigned, std::unique_ptr<Closure> >;

  ClosureMapType&                  closures()       { return d_closures; }
  const ClosureMapType&            closures() const { return d_closures; }

  const Closure                   *getClosure(unsigned) const;
  const Closure                   *getClosure(llvm::StringRef) const;

  //
  unsigned                         bxdf_address_space() const { return d_bxdf_address_space; }

  std::tuple<const BXDF *, unsigned, bool> getOrInsertBXDF(BXDF::EncodingView, BXDFAST::NodeRef, std::size_t);

  llvm::Module                    *bxdf_module()       { return d_bxdf_module.get(); }
  const llvm::Module              *bxdf_module() const { return d_bxdf_module.get(); }

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

private:

  void registerClosures();

  // OSL::RendererServices overrides:
  int supports(OSL::string_view) const override;

  OSLErrorHandler d_osl_error_handler;

  llvm::LLVMContext& d_llcontext;
  std::unique_ptr<llvm::Module> d_bxdf_module;

  llvm::StructType *d_string_type = nullptr;
  llvm::StructType *d_closure_type = nullptr;
  llvm::StructType *d_shader_globals_type = nullptr;

  std::unique_ptr<OSL::ShadingSystem> d_shading_system;
  OSL::ShadingContext *d_shading_context;

  ClosureMapType d_closures;
  llvm::StringMap<unsigned> d_closures_by_name;

  BuilderImpl *d_builder = nullptr;

  ShaderListType d_shaders;

  std::unordered_map<OSL::pvt::ShaderMaster::ref, Shader *> d_shader_masters;

  unsigned d_bxdf_address_space = 0;
  BXDFListType d_bxdfs;
  UberBXDFListType d_uber_bxdfs;

  UberBXDF *d_uber_bxdf = nullptr;

  std::map<BXDF::Encoding, const BXDF *, std::less<> > d_bxdf_index;
};

}

#endif
