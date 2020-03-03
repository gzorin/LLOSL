//-*-C++-*-
#ifndef LLOSL_SHADER_H
#define LLOSL_SHADER_H

#include <llvm/ADT/ilist_node.h>
#include <llvm/IR/TrackingMDRef.h>

#include <OpenImageIO/typedesc.h>
#include <OpenImageIO/ustring.h>

#include <memory>

namespace llvm {
class Constant;
class Function;
class Module;
class Type;
} // End namespace llvm

namespace OSL_v1_10 {
class ShaderGroup;
namespace pvt {
class ShaderMaster;
} // namespace pvt
} // End namespace OSL_v1_10

namespace OSL = OSL_v1_10;

namespace llosl {

class BuilderImpl;
class BXDF;
class BXDFAST;
class BXDFInfo;
class LLOSLContextImpl;

class Shader : public llvm::ilist_node<Shader> {
public:
    Shader()               = delete;
    Shader(const Shader &) = delete;
    Shader(Shader &&)      = delete;

    ~Shader();

    void removeFromContext();

    const llvm::Module *module() const { return d_module.get(); }
    llvm::Module *      module() { return d_module.get(); }

    const llvm::Function *main_function() const;
    const llvm::Function *closure_function() const;
    const llvm::Function *mapped_function() const;

    class Parameter {
    public:
        bool is_output() const { return d_is_output; }

        unsigned        index() const;
        llvm::StringRef name(unsigned) const;
        llvm::Type *    llvm_type() const;
        bool            is_closure() const { return d_is_closure; }
        OIIO::TypeDesc  osl_type() const;
        llvm::Constant *default_value() const { return d_default_value; }

    private:
        friend class Shader;

        Parameter(bool, llvm::Type *, unsigned, llvm::ArrayRef<OIIO::ustring>, bool,
                  const OIIO::TypeDesc &, llvm::Constant *, Shader *);
        ~Parameter() = default;

        Shader *                   d_parent    = nullptr;
        bool                       d_is_output = false;
        llvm::Type *               d_type      = nullptr;
        std::vector<OIIO::ustring> d_name;
        bool                       d_is_closure    = false;
        llvm::Constant *           d_default_value = nullptr;

        llvm::TypedTrackingMDRef<llvm::MDTuple> d_md;

        unsigned d_md_index = 0;
    };

    using parameter_iterator       = Parameter *;
    using const_parameter_iterator = const Parameter *;

    parameter_iterator       arg_begin() { return d_parameters; }
    const parameter_iterator arg_begin() const { return d_parameters; }

    parameter_iterator       arg_end() { return d_parameters + parameter_count(); }
    const parameter_iterator arg_end() const { return d_parameters + parameter_count(); }

    std::size_t parameter_count() const { return d_parameter_count; }

    // Information about the BXDFs computed by this shader:
    std::size_t    path_count() const;
    const BXDFAST *getBXDFForPath(std::size_t) const;
    std::size_t    getBXDFMaxHeapSize() const;

    llvm::MDNode *      metadata() { return d_md.get(); }
    const llvm::MDNode *metadata() const { return d_md.get(); }

private:
    class IRGenContext;

    Shader(LLOSLContextImpl &, OSL::ShaderGroup &);
    Shader(LLOSLContextImpl &, OSL::pvt::ShaderMaster &);

    void processBXDFs();
    void optimize();

    LLOSLContextImpl *d_context;

    std::unique_ptr<llvm::Module> d_module;

    std::size_t d_parameter_count = 0;
    Parameter * d_parameters      = nullptr;

    std::shared_ptr<const BXDFInfo> d_bxdf_info;
    std::vector<const BXDF *>       d_bxdfs;

    llvm::TypedTrackingMDRef<llvm::MDNode>          d_md;
    llvm::TypedTrackingMDRef<llvm::ValueAsMetadata> d_main_function_md, d_closure_function_md,
        d_mapped_function_md;
    llvm::TypedTrackingMDRef<llvm::MDTuple> d_parameters_md;
    llvm::TypedTrackingMDRef<llvm::MDTuple> d_bxdf_md;

    friend class BuilderImpl;
    friend class LLOSLContextImpl;
};

} // End namespace llosl

#endif
