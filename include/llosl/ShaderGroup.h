//-*-C++-*-
#ifndef LLOSL_SHADERGROUP_H
#define LLOSL_SHADERGROUP_H

#include <llvm/ADT/ilist_node.h>
#include <llvm/IR/TrackingMDRef.h>

#include <OpenImageIO/typedesc.h>
#include <OpenImageIO/ustring.h>

#include <memory>

namespace llvm {
class Function;
class Module;
class Type;
} // End namespace llvm

namespace llosl {

class BuilderImpl;
class BXDF;
class BXDFInfo;
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
    llvm::Type *parameters_type() const { return d_parameters_type; }

    const llvm::Function *init_function() const;
    const llvm::Function *main_function() const;

    class Parameter {
    public:

        unsigned index() const;
        llvm::StringRef name() const;
        llvm::Type *llvm_type() const;
        OIIO::TypeDesc osl_type() const;

    private:

        friend class ShaderGroup;

        Parameter(unsigned, const OIIO::ustring&, const OIIO::TypeDesc&, ShaderGroup *);
        ~Parameter() = default;

        ShaderGroup* d_parent = nullptr;

        llvm::TypedTrackingMDRef<llvm::MDTuple> d_md;
        unsigned d_md_index = 0;
    };

    using parameter_iterator = Parameter *;
    using const_parameter_iterator = const Parameter *;

    parameter_iterator arg_begin() { return d_parameters; }
    const parameter_iterator arg_begin() const { return d_parameters; }

    parameter_iterator arg_end() { return d_parameters + parameter_count(); }
    const parameter_iterator arg_end() const { return d_parameters + parameter_count(); }

    std::size_t parameter_count() const { return d_parameter_count; }

    std::size_t path_count() const;
    const BXDF *getBXDFForPath(std::size_t) const;
    std::size_t getBXDFMaxHeapSize() const;

    llvm::MDNode *metadata() { return d_md.get(); }
    const llvm::MDNode *metadata() const { return d_md.get(); }

private:

    ShaderGroup(BuilderImpl&);

    LLOSLContextImpl *d_context;

    std::unique_ptr<llvm::Module> d_module;
    llvm::Type *d_globals_type = nullptr;
    llvm::Type *d_data_type = nullptr;
    llvm::Type *d_parameters_type = nullptr;

    std::size_t d_parameter_count = 0;
    Parameter *d_parameters = nullptr;

    std::shared_ptr<const BXDFInfo> d_bxdf_info;

    llvm::TypedTrackingMDRef<llvm::MDNode> d_md;
    llvm::TypedTrackingMDRef<llvm::ValueAsMetadata> d_init_function_md, d_main_function_md;
    llvm::TypedTrackingMDRef<llvm::MDTuple> d_parameters_md;
    llvm::TypedTrackingMDRef<llvm::MDTuple> d_bxdf_md;

    friend class BuilderImpl;
};

} // End namespace llosl

#endif
