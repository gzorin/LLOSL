//-*-C++-*-
#ifndef LLOSL_LLOSLCONTEXT_H
#define LLOSL_LLOSLCONTEXT_H

#include <llvm/ADT/ilist.h>
#include <llvm/Support/Error.h>

#include <OSL/oslconfig.h>

#include <memory>
#include <system_error>

namespace llvm {
class Constant;
class LLVMContext;
class Type;
} // End namespace llvm

namespace llosl {

class Builder;
class BXDF;
class LLOSLContextImpl;
class Shader;
class ShaderGroup;
class UberBXDF;

class LLOSLContext {
public:

  static const std::error_category& ErrorCategory();

  enum class Error : int {
      AlreadyBuilding = 1
  };

  using ShaderListType = llvm::ilist<Shader>;

  using BXDFListType = llvm::ilist<BXDF>;
  using UberBXDFListType = llvm::ilist<UberBXDF>;
  using ShaderGroupListType = llvm::ilist<ShaderGroup>;

  static LLOSLContext       *Get(llvm::LLVMContext *);
  static const LLOSLContext *Get(const llvm::LLVMContext *);

  LLOSLContext(llvm::LLVMContext&, unsigned = 0);
  ~LLOSLContext();

  const llvm::LLVMContext&   getLLContext() const;
  llvm::LLVMContext&         getLLContext();

  llvm::Type                *getLLVMType(const OSL::TypeDesc&);
  llvm::Constant            *getLLVMDefaultConstant(const OSL::TypeDesc&);
  std::pair<llvm::Constant *, const void *> getLLVMConstant(const OSL::TypeDesc&, const void *);

  llvm::Expected<Builder>    getBuilder();

  UberBXDF                   *getUberBXDF() const;

private:

  struct ErrorCategory;

  std::unique_ptr<LLOSLContextImpl> d_impl;

  friend class LLOSLContextImpl;
};

} // End namespace llosl

namespace std {

template<> struct is_error_code_enum<llosl::LLOSLContext::Error> : std::true_type {};

} // End namespace std

#endif
