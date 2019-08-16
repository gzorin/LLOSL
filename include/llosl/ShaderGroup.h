//-*-C++-*-
#ifndef LLOSL_SHADERGROUP_H
#define LLOSL_SHADERGROUP_H

#include <llvm/ADT/ilist_node.h>
#include <llvm/IR/TrackingMDRef.h>

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

    llvm::Type *globals_type() const { return d_globals_type; }
    llvm::Type *data_type() const { return d_data_type; }

    const llvm::Function *init_function() const;
    const llvm::Function *main_function() const;

private:

    ShaderGroup(BuilderImpl&);

    LLOSLContextImpl *d_context;

    std::unique_ptr<llvm::Module> d_module;
    llvm::Type *d_globals_type = nullptr;
    llvm::Type *d_data_type = nullptr;

    llvm::TypedTrackingMDRef<llvm::MDNode> d_md;
    llvm::TypedTrackingMDRef<llvm::ValueAsMetadata> d_init_function_md, d_main_function_md;

    friend class BuilderImpl;
};

} // End namespace llosl

#endif
