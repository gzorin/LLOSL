//-*-C++-*-
#ifndef LLOSL_LLOSLCONTEXT_H
#define LLOSL_LLOSLCONTEXT_H

#include <llvm/ADT/StringRef.h>
#include <llvm/ADT/ilist.h>
#include <llvm/Support/Error.h>

#include <OpenImageIO/typedesc.h>

#include <memory>
#include <system_error>
namespace llvm {
class Constant;
class LLVMContext;
class StructType;
class Type;
} // End namespace llvm

namespace llosl {

class Builder;
class BXDF;
class LLOSLContextImpl;
class Shader;
class UberBXDF;

class LLOSLContext {
public:
    static const std::error_category &ErrorCategory();

    enum class Error : int { AlreadyBuilding = 1 };

    using ShaderListType = llvm::ilist<Shader>;

    using BXDFListType     = llvm::ilist<BXDF>;
    using UberBXDFListType = llvm::ilist<UberBXDF>;

    static LLOSLContext *      Get(llvm::LLVMContext *);
    static const LLOSLContext *Get(const llvm::LLVMContext *);

    LLOSLContext(llvm::LLVMContext &, unsigned = 0);
    ~LLOSLContext();

    const llvm::LLVMContext &getLLContext() const;
    llvm::LLVMContext &      getLLContext();

    llvm::Type *      getLLVMType(const OIIO::TypeDesc &, bool);
    llvm::StructType *getShaderGlobalsType();

    llvm::Constant *                          getLLVMDefaultConstant(const OIIO::TypeDesc &, bool);
    std::pair<llvm::Constant *, const void *> getLLVMConstant(const OIIO::TypeDesc &, const void *,
                                                              bool);

    llvm::Expected<Shader *> createShaderFromFile(llvm::StringRef);

    llvm::Expected<Builder> getBuilder();

    UberBXDF *getUberBXDF() const;

private:
    struct ErrorCategory;

    std::unique_ptr<LLOSLContextImpl> d_impl;

    friend class LLOSLContextImpl;
};

} // End namespace llosl

namespace std {

template <> struct is_error_code_enum<llosl::LLOSLContext::Error> : std::true_type {};

} // End namespace std

#endif
