//-*-C++-*-
#ifndef LLOSL_SHADER_H
#define LLOSL_SHADER_H

#include <llvm/ADT/ilist_node.h>
#include <llvm/IR/TrackingMDRef.h>

#include <OSL/oslexec.h>

#include <OpenImageIO/typedesc.h>
#include <OpenImageIO/ustring.h>

#include <memory>

namespace llvm {
class Function;
class Module;
class Type;
} // End namespace llvm

OSL_NAMESPACE_ENTER
class ShaderGroup;
namespace pvt {
class ShaderMaster;
} // End namespace OSL::pvt
OSL_NAMESPACE_EXIT

namespace llosl {

class BuilderImpl;
class LLOSLContextImpl;

class Shader : public llvm::ilist_node<Shader> {
public:

    Shader() = delete;
    Shader(const Shader&) = delete;
    Shader(Shader&&) = delete;

    ~Shader();

    void removeFromContext();

    const llvm::Module *module() const { return d_module.get(); }
    llvm::Module       *module()       { return d_module.get(); }

    llvm::Type *inputs_type() const { return d_inputs_type; }
    llvm::Type *outputs_type() const { return d_outputs_type; }

    const llvm::Function *main_function() const;

    class Parameter {
    public:

        unsigned index() const;
        llvm::StringRef name() const;
        llvm::Type *llvm_type() const;
        OIIO::TypeDesc osl_type() const;

    private:

        friend class Shader;

        Parameter(unsigned, const OIIO::ustring&, const OIIO::TypeDesc&, Shader *);
        ~Parameter() = default;

        Shader* d_parent = nullptr;

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

    llvm::MDNode *metadata() { return d_md.get(); }
    const llvm::MDNode *metadata() const { return d_md.get(); }

private:

    class IRGenContext;

    Shader(LLOSLContextImpl&, OSL::ShaderGroup&);
    Shader(LLOSLContextImpl&, OSL::pvt::ShaderMaster&);

    void StartProcessingShaderGroup(OSL::ShaderGroup&);
    void StopProcessingShaderGroup(OSL::ShaderGroup&);

    LLOSLContextImpl *d_context;

    std::unique_ptr<llvm::Module> d_module;
    llvm::Type *d_inputs_type = nullptr, *d_outputs_type = nullptr;

    std::size_t d_parameter_count = 0;
    Parameter *d_parameters = nullptr;

    llvm::TypedTrackingMDRef<llvm::MDNode> d_md;
    llvm::TypedTrackingMDRef<llvm::ValueAsMetadata> d_main_function_md;
    llvm::TypedTrackingMDRef<llvm::MDTuple> d_parameters_md;

    friend class BuilderImpl;
    friend class LLOSLContextImpl;
};

} // End namespace llosl

#endif
