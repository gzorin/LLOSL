// -*-C++-*-
#ifndef LLOSL_BXDFSCOPE_H
#define LLOSL_BXDFSCOPE_H

#include <llvm/ADT/ilist_node.h>

#include <memory>

namespace llosl {

class BXDFScopeImpl;
class LLOSLContext;
class LLOSLContextImpl;
class ShaderGroup;

class BXDFScope : public llvm::ilist_node<BXDFScope> {
public:

    BXDFScope(LLOSLContext * = nullptr);
    ~BXDFScope();

    BXDFScope(const BXDFScope&) = delete;
    BXDFScope(BXDFScope&&) = delete;

    void removeFromContext();

    void insertBXDFsFromShaderGroup(const ShaderGroup&);

private:

    LLOSLContextImpl *d_context;

    std::unique_ptr<BXDFScopeImpl> d_impl;
};

} // End namespace llosl

#endif
