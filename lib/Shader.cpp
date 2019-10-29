#include "BuilderImpl.h"
#include "LLOSLContextImpl.h"

#include <llosl/Shader.h>

#include <llvm/IR/Constants.h>
#include <llvm/IR/DerivedTypes.h>
#include <llvm/IR/Metadata.h>
#include <llvm/IR/Module.h>
#include <llvm/IR/Type.h>

#include <osl_pvt.h>
#include <oslexec_pvt.h>
#include <runtimeoptimize.h>

namespace llosl {

class Shader::IRGenContext {
public:

    IRGenContext(LLOSLContextImpl&);

    llvm::Type *getLLVMType(const OSL::pvt::TypeSpec&);

private:

    LLOSLContextImpl &d_context;
    llvm::LLVMContext& d_ll_context;

    llvm::Type *d_closure_type = nullptr;
    llvm::Type *d_triple_type = nullptr;
    llvm::Type *d_matrix_type = nullptr;
    llvm::Type *d_string_type = nullptr;
};

Shader::IRGenContext::IRGenContext(LLOSLContextImpl& context)
    : d_context(context)
    , d_ll_context(d_context.getLLContext()) {
}

llvm::Type *
Shader::IRGenContext::getLLVMType(const OSL::pvt::TypeSpec& t) {
    if (t.is_closure()) {
        if (!d_closure_type) {
            d_closure_type = llvm::StructType::get(d_ll_context,
                std::vector<llvm::Type *>{
                    llvm::PointerType::get(llvm::Type::getVoidTy(d_ll_context), 0)
                });
        }

        return d_closure_type;
    }

    if (t.is_void()) {
        return llvm::Type::getVoidTy(d_ll_context);
    }

    if (t.is_int()) {
        return llvm::Type::getInt32Ty(d_ll_context);
    }

    if (t.is_float()) {
        return llvm::Type::getFloatTy(d_ll_context);
    }

    if (t.is_triple()) {
        if (!d_triple_type) {
            d_triple_type = llvm::StructType::get(d_ll_context,
                std::vector<llvm::Type *>{
                    llvm::ArrayType::get(
                        llvm::Type::getFloatTy(d_ll_context), 3)
                });
        }

        return d_triple_type;
    }

    if (t.is_matrix()) {
        if (!d_matrix_type) {
            d_matrix_type = llvm::StructType::get(d_ll_context,
                std::vector<llvm::Type *>{
                    llvm::ArrayType::get(
                        llvm::ArrayType::get(
                            llvm::Type::getFloatTy(d_ll_context), 4), 4)
                });
        }

        return d_matrix_type;
    }

    if (t.is_string()) {
        if (!d_string_type) {
            d_string_type = llvm::StructType::get(d_ll_context,
                std::vector<llvm::Type *>{
                    llvm::Type::getInt32Ty(d_ll_context)
                });
        }

        return d_string_type;
    }

    return nullptr;
}

Shader::Shader(LLOSLContextImpl& context, OSL::ShaderGroup& shader_group)
    : d_context(&context) {
    auto& ll_context = d_context->getLLContext();

    d_context->addShader(this);

    StartProcessingShaderGroup(shader_group);
    d_module = std::make_unique<llvm::Module>("ShaderGroup", ll_context);

    // Order the 'layers' topologically:
    std::vector<OSL::pvt::ShaderInstance *> layers;

    struct Frame {
        int layer;
        bool back;
    };

    std::stack<Frame> stack;

    enum class Color {
        White, Grey, Black
    };

    llvm::DenseMap<int, Color> color;

    for (int i = 0, n = shader_group.nlayers(); i < n; ++i) {
        if (color[i] != Color::White) {
            continue;
        }

        stack.push({ i, false });

        while (!stack.empty()) {
            auto [ i, back ] = stack.top();
            stack.pop();

            auto layer = shader_group.layer(i);

            if (!back) {
                color[i] = Color::Grey;

                stack.push({ i, true });

                for (const auto& connection : layer->connections()) {
                    if (color[connection.srclayer] != Color::White) {
                        continue;
                    }

                    stack.push({ connection.srclayer, false });
                }
            }
            else {
                color[i] = Color::Black;

                layers.push_back(layer);
            }
        }
    }

    // Collect Shaders from ShaderMasters:
    std::vector<Shader *> shader_masters;

    std::transform(
        layers.begin(), layers.end(),
        std::back_inserter(shader_masters),
        [this](auto shader_instance) -> auto {
            auto shader = d_context->getShaderFromShaderMaster(shader_instance->master());
            assert(shader);
            return *shader;
        });

    StopProcessingShaderGroup(shader_group);
    d_module->dump();
}

Shader::Shader(LLOSLContextImpl& context, OSL::pvt::ShaderMaster& shader_master)
    : d_context(&context) {
    auto& ll_context = d_context->getLLContext();

    d_context->addShader(this);

    // Create a temporary group to perform optimizations on:
    OSL::ShaderGroupRef group(new OSL::ShaderGroup("tmp"));

    OSL::pvt::ShaderInstance::ref instance(new OSL::pvt::ShaderInstance(&shader_master));
    instance->parameters({});

    group->append(instance);

    StartProcessingShaderGroup(*group);
    d_module = std::make_unique<llvm::Module>(shader_master.shadername(), ll_context);

    IRGenContext irgen_context(*d_context);

    const auto& symbols = instance->symbols();

    // Count the number of inputs and outputs:
    unsigned input_count = 0, output_count = 0;

    std::for_each(
        symbols.begin(), symbols.end(),
        [&input_count, &output_count](const auto& symbol) -> void {
            auto s = symbol.dealias();

            if (s->symtype() == SymTypeParam) {
                ++input_count;
                return;
            }

            if (s->symtype() == SymTypeOutputParam) {
                ++output_count;
                return;
            }
        });

    // Create llvm::StructTypes for inputs and outputs:
    std::vector<llvm::Type *> input_types, output_types;
    input_types.reserve(input_count);
    output_types.reserve(output_count);

    std::for_each(
        symbols.begin(), symbols.end(),
        [&irgen_context, &input_types, &output_types](const auto& symbol) -> void {
            auto s = symbol.dealias();

            if (s->symtype() != SymTypeParam &&
                s->symtype() != SymTypeOutputParam) {
                return;
            }

            auto type = irgen_context.getLLVMType(s->typespec());

            if (s->symtype() == SymTypeParam) {
                input_types.push_back(type);
                return;
            }

            if (s->symtype() == SymTypeOutputParam) {
                output_types.push_back(type);
                return;
            }
        });

    d_inputs_type = llvm::StructType::get(ll_context, input_types);
    d_outputs_type = llvm::StructType::get(ll_context, output_types);

    StopProcessingShaderGroup(*group);
    d_module->dump();
}

Shader::~Shader() {
}

void
Shader::StartProcessingShaderGroup(OSL::ShaderGroup& shader_group) {
    OSL::pvt::RuntimeOptimizer rop(d_context->getShadingContext()->shadingsys(), shader_group, nullptr);
    rop.run();
}

void
Shader::StopProcessingShaderGroup(OSL::ShaderGroup& shader_group) {
    // Satisfy the invariants of OSL::ShaderGroup's destructor:
    for (int i = 0, n = shader_group.nlayers(); i < n; ++i) {
        auto layer = shader_group.layer(i);

        // We no longer needs ops and args -- create empty vectors and
        // swap with the ones in the instance.
        OpcodeVec emptyops;
        layer->ops().swap (emptyops);
        std::vector<int> emptyargs;
        layer->args().swap (emptyargs);
        if (layer->unused()) {
            // If we'll never use the layer, we don't need the syms at all
            SymbolVec nosyms;
            std::swap (layer->symbols(), nosyms);
        }
    }
}

void
Shader::removeFromContext() {
    if (d_context) {
	    d_context->removeShader(this);
        d_context = nullptr;
    }
}

const llvm::Function *
Shader::main_function() const {
    return d_main_function_md
      ? llvm::cast<llvm::Function>(d_main_function_md->getValue())
      : nullptr;
}

Shader::Parameter::Parameter(unsigned index, const OSL::ustring& name, const OSL::TypeDesc& type, Shader *parent)
: d_parent(parent) {
    auto& ll_context = d_parent->module()->getContext();

    d_md.reset(
        llvm::MDTuple::get(
        ll_context, {
            llvm::ConstantAsMetadata::get(
                llvm::ConstantInt::get(ll_context, llvm::APInt(32, index, true))),
            llvm::MDString::get(ll_context, "llosl.parameter_name"),
            llvm::MDString::get(ll_context, name.c_str()),
            llvm::MDString::get(ll_context, "llosl.type"),
            llvm::ConstantAsMetadata::get(
                llvm::ConstantInt::get(ll_context, llvm::APInt(8, type.basetype, true))),
            llvm::ConstantAsMetadata::get(
                llvm::ConstantInt::get(ll_context, llvm::APInt(8, type.aggregate, true))),
            llvm::ConstantAsMetadata::get(
                llvm::ConstantInt::get(ll_context, llvm::APInt(8, type.vecsemantics, true))),
            llvm::ConstantAsMetadata::get(
                llvm::ConstantInt::get(ll_context, llvm::APInt(32, type.arraylen, true)))
        }));
}

unsigned
Shader::Parameter::index() const {
    auto md = llvm::mdconst::dyn_extract<llvm::ConstantInt>(d_md->getOperand(0).get());
    assert(md);

	return md->getZExtValue();
}

llvm::StringRef
Shader::Parameter::name() const {
    return llvm::cast<llvm::MDString>(d_md->getOperand(2).get())->
	    getString();
}

llvm::Type *
Shader::Parameter::llvm_type() const {
    //return llvm::cast<llvm::StructType>(d_parent->data_type())->getTypeAtIndex(index());
    return nullptr;
}

OIIO::TypeDesc
Shader::Parameter::osl_type() const {
    auto basetype_md = llvm::mdconst::dyn_extract<llvm::ConstantInt>(d_md->getOperand(4).get());
    assert(basetype_md);

    auto aggregate_md = llvm::mdconst::dyn_extract<llvm::ConstantInt>(d_md->getOperand(5).get());
    assert(aggregate_md);

    auto vecsemantics_md = llvm::mdconst::dyn_extract<llvm::ConstantInt>(d_md->getOperand(6).get());
    assert(vecsemantics_md);

    auto arraylen_md = llvm::mdconst::dyn_extract<llvm::ConstantInt>(d_md->getOperand(7).get());
    assert(arraylen_md);

    return OIIO::TypeDesc(
        static_cast<OIIO::TypeDesc::BASETYPE>(basetype_md->getZExtValue()),
        static_cast<OIIO::TypeDesc::AGGREGATE>(aggregate_md->getZExtValue()),
        static_cast<OIIO::TypeDesc::VECSEMANTICS>(vecsemantics_md->getZExtValue()),
        static_cast<unsigned>(arraylen_md->getZExtValue())
    );
}

} // End namespace llosl
