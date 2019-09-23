//-*-C++-*-
#ifndef LLOSL_LLOSLCONTEXT_H
#define LLOSL_LLOSLCONTEXT_H

#include <llvm/ADT/ilist.h>
#include <llvm/Support/Error.h>

#include <memory>
#include <system_error>

namespace llvm {
class LLVMContext;
} // End namespace llvm

namespace llosl {

class Builder;
class BXDF;
class LLOSLContextImpl;
class ShaderGroup;
class UberBXDF;

class LLOSLContext {
public:

  static const std::error_category& ErrorCategory();

  enum class Error : int {
      AlreadyBuilding = 1
  };

  using BXDFListType = llvm::ilist<BXDF>;
  using UberBXDFListType = llvm::ilist<UberBXDF>;
  using ShaderGroupListType = llvm::ilist<ShaderGroup>;

  static LLOSLContext       *Get(llvm::LLVMContext *);
  static const LLOSLContext *Get(const llvm::LLVMContext *);

  LLOSLContext(llvm::LLVMContext&);
  ~LLOSLContext();

  const llvm::LLVMContext&   getLLContext() const;
  llvm::LLVMContext&         getLLContext();

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
