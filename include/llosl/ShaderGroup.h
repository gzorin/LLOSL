//-*-C++-*-
#ifndef LLOSL_SHADERGROUP_H
#define LLOSL_SHADERGROUP_H

#include <llvm/ADT/ilist_node.h>

#include <memory>

namespace llvm {
class Function;
class Module;
class Type;
} // End namespace llvm

namespace llosl {

class BuilderImpl;
class LLOSLContextImpl;

class ShaderGroup : public llvm::ilist_node<ShaderGroup> {
public:

    ShaderGroup() = delete;
    ShaderGroup(const ShaderGroup&) = delete;
    ShaderGroup(ShaderGroup&&) = delete;

    ~ShaderGroup();

    void removeFromContext();

    const llvm::Module *module() const { return d_module.get(); }
    llvm::Module       *module()       { return d_module.get(); }

    const llvm::Type *globals_type() const { return d_globals_type; }
    const llvm::Type *data_type() const { return d_data_type; }

    const llvm::Function *init_function() const { return d_init_function; }
    const llvm::Function *main_function() const { return d_main_function; }

private:

    ShaderGroup(BuilderImpl&);

    LLOSLContextImpl *d_context;

    std::unique_ptr<llvm::Module> d_module;
    llvm::Type *d_globals_type = nullptr;
    llvm::Type *d_data_type = nullptr;
    llvm::Function *d_init_function = nullptr;
    llvm::Function *d_main_function = nullptr;

    friend class BuilderImpl;
};

} // End namespace llosl

#endif
