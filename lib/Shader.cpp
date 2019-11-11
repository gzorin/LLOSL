#include "BuilderImpl.h"
#include "LLOSLContextImpl.h"

#include <llosl/Shader.h>

#include <llvm/IR/BasicBlock.h>
#include <llvm/IR/Constants.h>
#include <llvm/IR/DerivedTypes.h>
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/Metadata.h>
#include <llvm/IR/Module.h>
#include <llvm/IR/Type.h>

#include <osl_pvt.h>
#include <oslexec_pvt.h>
#include <runtimeoptimize.h>

namespace llosl {

class Shader::IRGenContext {
public:

    IRGenContext(LLOSLContextImpl&, llvm::Module&);

    llvm::Type     *getLLVMType(const OSL::pvt::TypeSpec&);
    std::pair<llvm::Constant *, const void *> getLLVMConstant(const OSL::pvt::TypeSpec&, const void *);
    llvm::Constant *getLLVMConstant(const Symbol&);

    void beginFunction(llvm::StringRef, llvm::Type *, llvm::Type *);
    std::unique_ptr<llvm::Function> endFunction();

    llvm::Function *function() const { assert(d_state == State::Function); return d_function.get(); }
    llvm::Value    *inputs()   const { assert(d_state == State::Function); return d_inputs;         }
    llvm::Value    *outputs()  const { assert(d_state == State::Function); return d_outputs;        }

    void insertSymbolAddress(const Symbol&, llvm::Value *);
    void insertSymbolValue(const Symbol&, llvm::Value *);

private:

    enum class State {
        Initial,
        Function,
        Final
    };

    LLOSLContextImpl &d_context;
    llvm::LLVMContext& d_ll_context;
    llvm::Module& d_module;

    State d_state = State::Initial;

    llvm::Type *d_closure_type = nullptr;
    llvm::Type *d_triple_type = nullptr;
    llvm::Type *d_matrix_type = nullptr;
    llvm::Type *d_string_type = nullptr;
    llvm::DenseMap<int, llvm::Type *> d_struct_types;

    std::unique_ptr<llvm::Function> d_function;
    llvm::Value *d_inputs = nullptr, *d_outputs = nullptr;
    llvm::DenseMap<const Symbol *, llvm::Value *> d_symbol_addresses, d_symbol_values;
};

Shader::IRGenContext::IRGenContext(LLOSLContextImpl& context, llvm::Module& module)
    : d_context(context)
    , d_ll_context(d_context.getLLContext())
    , d_module(module) {
}

llvm::Type *
Shader::IRGenContext::getLLVMType(const OSL::pvt::TypeSpec& t) {
    if (t.is_closure()) {
        if (!d_closure_type) {
            d_closure_type = llvm::StructType::get(d_ll_context,
                std::vector<llvm::Type *>{
                    llvm::PointerType::get(
                        llvm::Type::getInt8Ty(d_ll_context), 0)
                });
        }

        return d_closure_type;
    }

    if (t.is_float()) {
        return llvm::Type::getFloatTy(d_ll_context);
    }

    if (t.is_triple()) {
        if (!d_triple_type) {
            d_triple_type = llvm::VectorType::get(
                llvm::Type::getFloatTy(d_ll_context), 3);
        }

        return d_triple_type;
    }

    if (t.is_int()) {
        return llvm::Type::getInt32Ty(d_ll_context);
    }

    if (t.is_matrix()) {
        if (!d_matrix_type) {
            d_matrix_type = llvm::StructType::get(d_ll_context,
                std::vector<llvm::Type *>{
                    llvm::ArrayType::get(
                        llvm::VectorType::get(
                            llvm::Type::getFloatTy(d_ll_context), 4), 4)
                });
        }

        return d_matrix_type;
    }

    if (t.is_structure()) {
        if (d_struct_types.count(t.structure()) == 0) {
            auto struct_spec = t.structspec();
            std::vector<llvm::Type *> member_types;
            member_types.reserve(struct_spec->numfields());

            for (int i = 0, n = struct_spec->numfields(); i < n; ++i) {
                member_types.push_back(
                    getLLVMType(struct_spec->field(i).type));
            }

            auto struct_type = llvm::StructType::get(d_ll_context, member_types);
            d_struct_types[t.structure()] = struct_type;
        }

        return d_struct_types[t.structure()];
    }

    if (t.is_sized_array()) {
        return llvm::ArrayType::get(
            getLLVMType(t.elementtype()), t.arraylength());
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

    if (t.is_void()) {
        return llvm::Type::getVoidTy(d_ll_context);
    }

    return nullptr;
}

std::pair<llvm::Constant *, const void *>
Shader::IRGenContext::getLLVMConstant(const OSL::pvt::TypeSpec& t, const void *data) {
    auto llvm_type = getLLVMType(t);

    if (t.is_closure()) {
        auto llvm_struct_type = llvm::cast<llvm::StructType>(llvm_type);

        return {
            llvm::ConstantStruct::get(
                llvm_struct_type,
                std::vector<llvm::Constant *>{
                    llvm::ConstantPointerNull::get(
                        llvm::PointerType::get(
                            llvm::Type::getInt8Ty(d_ll_context), 0))
                }),
            data };
    }

    if (t.is_float()) {
        const float *pfloat = reinterpret_cast<const float *>(data);

        return {
            llvm::ConstantFP::get(
                llvm_type, pfloat ? *pfloat : 0.0),
            pfloat ? pfloat + 1 : nullptr };
    }

    if (t.is_triple()) {
        struct float3 {
            float value[3];
        };

        const float3 *pfloat3 = reinterpret_cast<const float3 *>(data);

        static float3 zero = { { 0.0, 0.0, 0.0 } };

        return {
            llvm::ConstantDataVector::get(
                d_ll_context, pfloat3 ? pfloat3->value : zero.value),
            pfloat3 ? pfloat3 + 1 : nullptr };
    }

    if (t.is_int()) {
        const int *pint = reinterpret_cast<const int *>(data);

        return {
            llvm::ConstantInt::get(
                llvm_type, pint ? *pint : 0),
            pint ? pint + 1 : nullptr };
    }

    if (t.is_matrix()) {
        auto llvm_array_type = llvm::cast<llvm::ArrayType>(llvm_type);

        struct float4 {
            float value[4];
        };

        const float4 *pfloat4 = reinterpret_cast<const float4 *>(data);

        static float4 identity[] = {
            { { 1.0, 0.0, 0.0, 0.0 } },
            { { 0.0, 1.0, 0.0, 0.0 } },
            { { 0.0, 0.0, 1.0, 0.0 } },
            { { 0.0, 0.0, 0.0, 1.0 } },
        };

        return {
            llvm::ConstantArray::get(
                llvm_array_type, std::vector<llvm::Constant*>{
                    llvm::ConstantDataVector::get(d_ll_context, pfloat4 ? pfloat4[0].value : identity[0].value),
                    llvm::ConstantDataVector::get(d_ll_context, pfloat4 ? pfloat4[1].value : identity[1].value),
                    llvm::ConstantDataVector::get(d_ll_context, pfloat4 ? pfloat4[2].value : identity[2].value),
                    llvm::ConstantDataVector::get(d_ll_context, pfloat4 ? pfloat4[3].value : identity[3].value) }),
            pfloat4 ? pfloat4 + 1 : nullptr };
    }

    if (t.is_structure()) {
        auto llvm_struct_type = llvm::cast<llvm::StructType>(llvm_type);

        std::vector<llvm::Constant *> members;

        auto struct_spec = t.structspec();

        for (int i = 0, n = struct_spec->numfields(); i < n; ++i) {
            auto [ constant, next_data ] = getLLVMConstant(struct_spec->field(i).type, data);
            members.push_back(constant);
            data = next_data;
        }

        return {
            llvm::ConstantStruct::get(
                llvm_struct_type, members),
            data };
    }

    if (t.is_sized_array()) {
        auto llvm_array_type = llvm::cast<llvm::ArrayType>(llvm_type);

        std::vector<llvm::Constant *> elements;

        for (int i = 0, n = t.arraylength(); i < n; ++i) {
            auto [ constant, next_data ] = getLLVMConstant(t.elementtype(), data);
            elements.push_back(constant);
            data = next_data;
        }

        return {
            llvm::ConstantArray::get(
                llvm_array_type, elements),
            data };
    }

    if (t.is_string()) {
        auto llvm_struct_type = llvm::cast<llvm::StructType>(llvm_type);

        return {
            llvm::ConstantStruct::get(
                llvm_struct_type,
                std::vector<llvm::Constant *>{
                    llvm::ConstantInt::get(
                        llvm::Type::getInt32Ty(d_ll_context), 0xFFFF)
                }),
            data };
    }

    return { nullptr, data };
}

llvm::Constant *
Shader::IRGenContext::getLLVMConstant(const Symbol& s) {
    return getLLVMConstant(s.typespec(), s.data()).first;
}

void
Shader::IRGenContext::beginFunction(llvm::StringRef name, llvm::Type *inputs_type, llvm::Type *outputs_type) {
    assert(d_state == State::Initial);

    d_function.reset(llvm::Function::Create(
        llvm::FunctionType::get(
            llvm::Type::getVoidTy(d_ll_context),
                std::vector<llvm::Type *>{ llvm::PointerType::get(inputs_type,  0),
                                           llvm::PointerType::get(outputs_type, 0) },
            false),
        llvm::GlobalValue::ExternalLinkage, name, &d_module));

    d_inputs = d_function->arg_begin();
    d_inputs->setName("inputs");

    d_outputs = d_function->arg_begin() + 1;
    d_outputs->setName("outputs");

    d_state = State::Function;
}

std::unique_ptr<llvm::Function>
Shader::IRGenContext::endFunction() {
    assert(d_state == State::Function);

    d_state = State::Final;

    return std::move(d_function);
}

void
Shader::IRGenContext::insertSymbolAddress(const Symbol& symbol, llvm::Value *value) {
    d_symbol_addresses[&symbol] = value;
}

void
Shader::IRGenContext::insertSymbolValue(const Symbol& symbol, llvm::Value *value) {
    d_symbol_values[&symbol] = value;
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

    IRGenContext irgen_context(*d_context, *d_module);

    const auto& symbols = instance->symbols();

    // Count the number of inputs and outputs:
    unsigned input_count = 0, output_count = 0;

    std::for_each(
        symbols.begin(), symbols.end(),
        [&input_count, &output_count](const auto& symbol) -> void {
            auto s = symbol.dealias();

            switch (s->symtype()) {
                case SymTypeParam:
                    ++input_count;
                    return;
                case SymTypeOutputParam:
                    ++output_count;
                    return;
                default:
                    break;
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

            switch (s->symtype()) {
                case SymTypeParam:
                    input_types.push_back(irgen_context.getLLVMType(s->typespec()));
                    return;
                case SymTypeOutputParam:
                    output_types.push_back(irgen_context.getLLVMType(s->typespec()));
                    return;
                default:
                    break;
            }
        });

    d_inputs_type = llvm::StructType::get(ll_context, input_types);
    d_outputs_type = llvm::StructType::get(ll_context, output_types);

    irgen_context.beginFunction(
        shader_master.shadername(), d_inputs_type, d_outputs_type);

    llvm::IRBuilder<> builder(ll_context);

    auto entry_block = llvm::BasicBlock::Create(ll_context, "entry", irgen_context.function());
    builder.SetInsertPoint(entry_block);

    unsigned input_index = 0, output_index = 0;

    std::for_each(
        symbols.begin(), symbols.end(),
        [&irgen_context, &builder,
         &input_index, &output_index](const auto& symbol) -> void {
            auto s = symbol.dealias();
            if (!s->everused()) {
                return;
            }

            auto name = s->name().string();

            switch (s->symtype()) {
                case SymTypeParam: {
                    // Inputs that aren't overwritten can be read once:
                    if (!s->everwritten()) {
                        auto address = builder.CreateStructGEP(nullptr, irgen_context.inputs(), input_index++);
                        auto value = builder.CreateLoad(address, name);
                        irgen_context.insertSymbolValue(*s, value);
                        break;
                    }

                    // Otherwise, store the address of the input:
                    auto address = builder.CreateStructGEP(nullptr, irgen_context.inputs(), input_index++, name);
                    irgen_context.insertSymbolAddress(*s, address);
                } break;
                case SymTypeOutputParam: {
                    // Outputs will presuambly be written to, so store their address, too:
                    auto value = builder.CreateStructGEP(nullptr, irgen_context.outputs(), output_index++, name);
                    irgen_context.insertSymbolAddress(*s, value);
                } break;
                case SymTypeLocal:
                case SymTypeTemp: {
                    // Locals need both an address and an initial value:
                    auto address = builder.CreateAlloca(irgen_context.getLLVMType(s->typespec()), nullptr, name);
                    auto value = irgen_context.getLLVMConstant(*s);
                    assert(value);

                    builder.CreateStore(value, address);
                } break;
                case SymTypeConst: {
                    // Constants are inlined:
                    auto value = irgen_context.getLLVMConstant(*s);
                    if (!value) {
                        break;
                    }

                    irgen_context.insertSymbolValue(*s, value);
                } break;
                default:
                    break;
            }
        });

    auto function = irgen_context.endFunction();

    d_main_function_md.reset(
        llvm::ValueAsMetadata::get(function.release()));

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
