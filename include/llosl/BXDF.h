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
    using Encoding     = std::basic_string<uint8_t>;
    using EncodingView = std::basic_string_view<uint8_t>;

    ~BXDF();

    BXDF()             = delete;
    BXDF(const BXDF &) = delete;
    BXDF(BXDF &&)      = delete;

    const EncodingView encoding() const { return d_encoding; }

    llvm::Type *    scope_type() const { return d_scope_type; }
    llvm::Function *function() const { return d_function; }

    std::size_t heap_size() const { return d_heap_size; }

private:
    BXDF(LLOSLContextImpl &, EncodingView, BXDFAST::NodeRef, std::size_t);

    LLOSLContextImpl *d_context;
    Encoding          d_encoding;
    llvm::Type *      d_scope_type = nullptr;
    llvm::Function *  d_function   = nullptr;
    std::size_t       d_heap_size  = 0;

    friend class LLOSLContextImpl;
};

} // End namespace llosl

#endif
