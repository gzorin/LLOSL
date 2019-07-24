//-*-C++-*-
#ifndef LLOSL_SHADERGROUP_H
#define LLOSL_SHADERGROUP_H

#include <llvm/ADT/ilist_node.h>

#include <memory>

namespace llvm {
class Module;
} // End namespace llvm

namespace llosl {

class BuilderImpl;

class ShaderGroup : public llvm::ilist_node<ShaderGroup> {
public:

    ShaderGroup() = delete;
    ShaderGroup(const ShaderGroup&) = delete;
    ShaderGroup(ShaderGroup&&) = delete;

    ~ShaderGroup();

private:

    ShaderGroup(BuilderImpl&);

    std::unique_ptr<llvm::Module> d_module;

    friend class BuilderImpl;
};

} // End namespace llosl

#endif
