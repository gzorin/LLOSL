//-*-C++-*-
#ifndef LLOSL_BXDF_H
#define LLOSL_BXDF_H

#include <llosl/IR/BXDFAST.h>

#include <llvm/ADT/ilist_node.h>

#include <string>
#include <string_view>

namespace llvm {
class Function;
class Type;
} // End namespace llvm

namespace llosl {

class LLOSLContextImpl;

class BXDF : public llvm::ilist_node<BXDF> {
public:

    using Encoding = std::basic_string<uint8_t>;
    using EncodingView = std::basic_string_view<uint8_t>;

    ~BXDF();

    BXDF() = delete;
    BXDF(const BXDF&) = delete;
    BXDF(BXDF&&) = delete;

    llvm::Type *scope_type()               { return d_scope_type; }
    const llvm::Type *scope_type() const   { return d_scope_type; }

    llvm::Function *function()             { return d_function;   }
    const llvm::Function *function() const { return d_function;   }

private:

    BXDF(LLOSLContextImpl&, EncodingView, BXDFAST::NodeRef);

    LLOSLContextImpl *d_context;
    llvm::Type *d_scope_type = nullptr;
    llvm::Function *d_function = nullptr;

    friend class LLOSLContextImpl;
};

} // End namespace llosl

#endif
