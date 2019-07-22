//-*-C++-*-
#ifndef LLOSL_LLOSLCONTEXT_H
#define LLOSL_LLOSLCONTEXT_H

#include <llvm/Support/Error.h>

#include <memory>
#include <system_error>

namespace llvm {
class LLVMContext;
} // End namespace llvm

namespace llosl {

class Builder;
class LLOSLContextImpl;

class LLOSLContext {
public:

  static const std::error_category& ErrorCategory();

  enum class Error : int {
      AlreadyBuilding = 1
  };

  static LLOSLContext       *Get(llvm::LLVMContext *);
  static const LLOSLContext *Get(const llvm::LLVMContext *);

  LLOSLContext(llvm::LLVMContext&);
  ~LLOSLContext();

  const llvm::LLVMContext&   getLLContext() const;
  llvm::LLVMContext&         getLLContext();

  llvm::Expected<Builder>    getBuilder();

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
