#include "BuilderImpl.h"
#include "LLOSLContextImpl.h"

#include <llosl/Shader.h>

#include <llvm/IR/BasicBlock.h>
#include <llvm/IR/CFG.h>
#include <llvm/IR/DerivedTypes.h>
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/Instruction.h>
#include <llvm/IR/Metadata.h>
#include <llvm/IR/Module.h>
#include <llvm/IR/Type.h>
#include <llvm/Support/FormatVariadic.h>

#include <osl_pvt.h>
#include <oslexec_pvt.h>
#include <runtimeoptimize.h>

#include <map>
#include <numeric>
#include <stack>

namespace { namespace Ops {

ustring u_end("end");
ustring u_nop("nop");
ustring u_functioncall("functioncall");
ustring u_return("return");
ustring u_if("if");
ustring u_for("for");
ustring u_while("while");
ustring u_dowhile("dowhile");
ustring u_break("break");
ustring u_continue("continue");
ustring u_assign("assign");
ustring u_add("add");
ustring u_sub("sub");
ustring u_mul("mul");
ustring u_div("div");
ustring u_mod("mod");
ustring u_neg("neg");
ustring u_eq("eq");
ustring u_neq("neq");
ustring u_lt("lt");
ustring u_gt("gt");
ustring u_le("le");
ustring u_ge("ge");
ustring u_bitand("bitand");
ustring u_bitor("bitor");
ustring u_xor("xor");
ustring u_shl("shl");
ustring u_shr("shr");
ustring u_compl("compl");

} }

namespace {

struct OSLScalarTypeTraits {
    static const OSLScalarTypeTraits& get(OSL::TypeDesc::BASETYPE);
    static const OSLScalarTypeTraits& get(const OSL::TypeDesc&);

    OSL::TypeDesc::BASETYPE getOSLScalarType() const;
    OSL::TypeDesc           getOSLTypeDesc(OSL::TypeDesc::AGGREGATE = OSL::TypeDesc::SCALAR) const;

    enum Type {
        NaN, Integer, Real
    };

    Type type = NaN;
    bool sign = false;
    std::size_t width = 0;
};

const OSLScalarTypeTraits&
OSLScalarTypeTraits::get(OSL::TypeDesc::BASETYPE basetype) {
    static const OSLScalarTypeTraits s_traits[] = {
        { },
        { },
        { OSLScalarTypeTraits::Integer, false,  8 },
        { OSLScalarTypeTraits::Integer, true,   8 },
        { OSLScalarTypeTraits::Integer, false, 16 },
        { OSLScalarTypeTraits::Integer, true,  16 },
        { OSLScalarTypeTraits::Integer, false, 32 },
        { OSLScalarTypeTraits::Integer, true,  32 },
        { OSLScalarTypeTraits::Integer, false, 64 },
        { OSLScalarTypeTraits::Integer, true,  64 },
        { OSLScalarTypeTraits::Real,    true,  16 },
        { OSLScalarTypeTraits::Real,    true,  32 },
        { OSLScalarTypeTraits::Real,    true,  64 },
        { },
        { }
    };

    return s_traits[basetype];
}

const OSLScalarTypeTraits&
OSLScalarTypeTraits::get(const OSL::TypeDesc& t) {
    return OSLScalarTypeTraits::get((OSL::TypeDesc::BASETYPE)t.basetype);
}

OSL::TypeDesc::BASETYPE
OSLScalarTypeTraits::getOSLScalarType() const {
    switch (type) {
        case OSLScalarTypeTraits::Integer: {
            switch (width) {
                case 8:
                    return sign ? OSL::TypeDesc::INT8  : OSL::TypeDesc::UINT8;
                case 16:
                    return sign ? OSL::TypeDesc::INT16 : OSL::TypeDesc::UINT16;
                case 32:
                    return sign ? OSL::TypeDesc::INT32 : OSL::TypeDesc::UINT32;
                case 64:
                    return sign ? OSL::TypeDesc::INT64 : OSL::TypeDesc::UINT64;
                default:
                    break;
            }
        } break;
        case OSLScalarTypeTraits::Real: {
            switch (width) {
                case 16:
                    return OSL::TypeDesc::HALF;
                case 32:
                    return OSL::TypeDesc::FLOAT;
                case 64:
                    return OSL::TypeDesc::DOUBLE;
                default:
                    break;
            }
        } break;
        default:
            break;
    }

    return OSL::TypeDesc::UNKNOWN;
}

OSL::TypeDesc
OSLScalarTypeTraits::getOSLTypeDesc(OSL::TypeDesc::AGGREGATE aggregate) const {
    return OSL::TypeDesc(getOSLScalarType(), aggregate);
}

OSL::TypeDesc
getPromotionType(llvm::ArrayRef<OSL::TypeDesc> types) {
    struct Traits {
        OSLScalarTypeTraits      basetype;
        OSL::TypeDesc::AGGREGATE aggregate = OSL::TypeDesc::SCALAR;
    };

    auto traits =
        std::accumulate(
            types.begin(), types.end(),
            std::optional<Traits>(),
            [](auto acc, auto cur) -> std::optional<Traits> {
                Traits traits = {
                    OSLScalarTypeTraits::get(cur),
                    (OSL::TypeDesc::AGGREGATE)cur.aggregate
                };

                if (!acc) {
                    return { traits };
                }

                acc->basetype.type  = std::max(acc->basetype.type,  traits.basetype.type);
                acc->basetype.sign  = std::max(acc->basetype.sign,  traits.basetype.sign);
                acc->basetype.width = std::max(acc->basetype.width, traits.basetype.width);
                acc->aggregate      = std::max(acc->aggregate,      traits.aggregate);

                return acc;
            });

    assert(traits);

    return traits->basetype.getOSLTypeDesc(traits->aggregate);
}

}

namespace llosl {

class Shader::IRGenContext {
public:

    IRGenContext(LLOSLContextImpl&, llvm::Module&);

    llvm::Type     *getLLVMType(const OSL::pvt::TypeSpec&);

    llvm::PointerType *getClosureType();

    using ShaderGlobalsIndex = std::map<OSL::ustring, unsigned>;

    llvm::Type                *getShaderGlobalsType();
    const ShaderGlobalsIndex&  getShaderGlobalsIndex();

    llvm::Constant *getLLVMDefaultConstant(const OSL::pvt::TypeSpec&);

    std::pair<llvm::Constant *, const void *> getLLVMConstant(const OSL::pvt::TypeSpec&, const void *);
    llvm::Constant *getLLVMConstant(const Symbol&);

    void beginFunction(llvm::StringRef, llvm::Type *, llvm::ArrayRef<llvm::Type *>);
    std::unique_ptr<llvm::Function> endFunction();

    llvm::Function *function() const {
        assert(d_state == State::Function || d_state == State::Block);

        return d_function.get();
    }

    void insertSymbolAddress(const Symbol&, llvm::Value *);
    void insertSymbolValue(const Symbol&, llvm::Value *);
    llvm::Value *getSymbolAddress(const Symbol&);

    llvm::Value *getSymbolValue(llvm::IRBuilder<>&, const Symbol&);
    llvm::Value *getSymbolConditionValue(llvm::IRBuilder<>&, const Symbol&);

    llvm::Value *convertValue(llvm::IRBuilder<>&, const OSL::TypeDesc&, const OSL::TypeDesc&, llvm::Value *);

    llvm::BasicBlock *createBlock(llvm::StringRef = llvm::StringRef());
    void beginBlock(llvm::BasicBlock *);
    void continueBlock(llvm::BasicBlock *);
    void endBlock();

    llvm::BasicBlock *block() const {
        assert(d_state == State::Block);
        assert(d_block);

        return d_block;
    }

    llvm::IRBuilder<>& builder() const {
        assert(d_builder);

        return *d_builder;
    }

    llvm::Value *getSymbolValue(const Symbol& symbol) {
        return getSymbolValue(builder(), symbol);
    }

    llvm::Value *getSymbolConditionValue(const Symbol& symbol) {
        return getSymbolConditionValue(builder(), symbol);
    }

    llvm::Value *convertValue(const OSL::TypeDesc& lhs_type, const OSL::TypeDesc& rhs_type, llvm::Value *rhs_value) {
        return convertValue(builder(), lhs_type, rhs_type, rhs_value);
    }

private:

    enum class State {
        Initial,
        Function,
        Block,
        Final
    };

    LLOSLContextImpl &d_context;
    llvm::LLVMContext& d_ll_context;
    llvm::Module& d_module;

    State d_state = State::Initial;

    llvm::PointerType *d_closure_type = nullptr;
    llvm::Type *d_string_type = nullptr;
    llvm::DenseMap<int, llvm::Type *> d_struct_types;
    llvm::Type *d_shaderglobals_type = nullptr;

    std::unique_ptr<llvm::Function> d_function;
    llvm::DenseMap<const Symbol *, llvm::Value *> d_symbol_addresses, d_symbol_values;

    llvm::BasicBlock *d_block = nullptr;
    std::unique_ptr<llvm::IRBuilder<> > d_builder;
};

Shader::IRGenContext::IRGenContext(LLOSLContextImpl& context, llvm::Module& module)
    : d_context(context)
    , d_ll_context(d_context.getLLContext())
    , d_module(module) {
}

llvm::Type *
Shader::IRGenContext::getLLVMType(const OSL::pvt::TypeSpec& t) {
    if (t.is_closure()) {
        return getClosureType();
    }

    if (t.is_structure_based()) {
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

        if (t.is_structure_array()) {
            return llvm::ArrayType::get(
                d_struct_types[t.structure()], t.arraylength());
        }

        return d_struct_types[t.structure()];
    }

    return d_context.getLLVMType(t.simpletype());
}

llvm::PointerType *
Shader::IRGenContext::getClosureType() {
    if (!d_closure_type) {
        d_closure_type = llvm::PointerType::get(
            llvm::Type::getInt8Ty(d_ll_context), 0);
    }

    return d_closure_type;
}

llvm::Type *
Shader::IRGenContext::getShaderGlobalsType() {
    if (!d_shaderglobals_type) {
        d_shaderglobals_type = llvm::StructType::create(
            d_ll_context,
            std::vector<llvm::Type *>{
                getLLVMType(TypeDesc::TypePoint),       // P
                getLLVMType(TypeDesc::TypePoint),       // dPdx
                getLLVMType(TypeDesc::TypePoint),       // dPdy
                getLLVMType(TypeDesc::TypePoint),       // dPdz
                getLLVMType(TypeDesc::TypeVector),      // I
                getLLVMType(TypeDesc::TypeVector),      // dIdx
                getLLVMType(TypeDesc::TypeVector),      // dIdy
                getLLVMType(TypeDesc::TypeNormal),      // N
                getLLVMType(TypeDesc::TypeNormal),      // Ng
                getLLVMType(TypeDesc::TypeFloat),       // u
                getLLVMType(TypeDesc::TypeFloat),       // dudx
                getLLVMType(TypeDesc::TypeFloat),       // dudy
                getLLVMType(TypeDesc::TypeFloat),       // v
                getLLVMType(TypeDesc::TypeFloat),       // dvdx
                getLLVMType(TypeDesc::TypeFloat),       // dvdy
                getLLVMType(TypeDesc::TypeVector),      // dPdu
                getLLVMType(TypeDesc::TypeVector),      // dPdv
                getLLVMType(TypeDesc::TypeFloat),       // time
                getLLVMType(TypeDesc::TypeFloat),       // dtime
                getLLVMType(TypeDesc::TypeVector),      // dPdtime
                llvm::PointerType::get(
                        llvm::Type::getInt8Ty(d_ll_context), 0),            // context
                getLLVMType(TypeDesc::TypeMatrix),      // object2common
                getLLVMType(TypeDesc::TypeMatrix),      // shader2common
                getLLVMType(OSL::pvt::TypeSpec(TypeDesc::TypeColor, true)), // Ci
            },
            "OSL::ShaderGlobals");
    }

    return d_shaderglobals_type;
}

const Shader::IRGenContext::ShaderGlobalsIndex&
Shader::IRGenContext::getShaderGlobalsIndex() {
    static ShaderGlobalsIndex index = {
        { OSL::ustring("P"),              0 },
        { OSL::ustring("I"),              4 },
        { OSL::ustring("N"),              7 },
        { OSL::ustring("Ng"),             8 },
        { OSL::ustring("u"),              9 },
        { OSL::ustring("v"),             12 },
        { OSL::ustring("dPdu"),          15 },
        { OSL::ustring("dPdv"),          16 },
        { OSL::ustring("time"),          17 },
        { OSL::ustring("dtime"),         18 },
        { OSL::ustring("dPdtime"),       19 },
        { OSL::ustring("object2common"), 21 },
        { OSL::ustring("shader2common"), 22 },
        { OSL::ustring("Ci"),            23 }
    };

    return index;
}

llvm::Constant *
Shader::IRGenContext::getLLVMDefaultConstant(const OSL::pvt::TypeSpec& t) {
    auto llvm_type = getLLVMType(t);

    if (t.is_closure()) {
        // TODO
        return
            llvm::ConstantPointerNull::get(getClosureType());
    }

    if (t.is_structure_array()) {
        auto struct_spec = t.structspec();
        OSL::pvt::TypeSpec element_type(nullptr, t.structure());

        std::vector<llvm::Constant *> elements(
            t.arraylength(), getLLVMDefaultConstant(element_type));

        return
            llvm::ConstantArray::get(
                llvm::cast<llvm::ArrayType>(llvm_type), elements);
    }

    if (t.is_structure_based()) {
        auto struct_spec = t.structspec();

        std::vector<llvm::Constant *> members(struct_spec->numfields(), nullptr);
        int i = 0;

        std::generate_n(
            members.begin(), struct_spec->numfields(),
            [this, struct_spec, &i]() -> llvm::Constant * {
                return getLLVMDefaultConstant(struct_spec->field(i++).type);
            });

        return
            llvm::ConstantStruct::get(
                llvm::cast<llvm::StructType>(llvm_type),
                members);
    }

    return d_context.getLLVMDefaultConstant(t.simpletype());
}

std::pair<llvm::Constant *, const void *>
Shader::IRGenContext::getLLVMConstant(const OSL::pvt::TypeSpec& t, const void *p) {
    auto llvm_type = getLLVMType(t);

    if (t.is_closure()) {
        // TODO
        return {
            llvm::ConstantPointerNull::get(getClosureType()),
            p
        };
    }

    if (t.is_structure_array()) {
        auto struct_spec = t.structspec();
        OSL::pvt::TypeSpec element_type(nullptr, t.structure());

        std::vector<llvm::Constant *> elements(t.arraylength(), nullptr);

        std::generate_n(
            elements.begin(), t.arraylength(),
            [this, &p, &element_type]() -> llvm::Constant * {
                auto [ element, next_p ] = getLLVMConstant(element_type, p);
                p = next_p;
                return element;
            });

        return {
            llvm::ConstantArray::get(
                llvm::cast<llvm::ArrayType>(llvm_type), elements),
            p
        };
    }

    if (t.is_structure_based()) {
        auto struct_spec = t.structspec();

        std::vector<llvm::Constant *> members(struct_spec->numfields(), nullptr);
        int i = 0;

        std::generate_n(
            members.begin(), struct_spec->numfields(),
            [this, &p, struct_spec, &i]() -> llvm::Constant * {
                auto [ member, next_p ] = getLLVMConstant(struct_spec->field(i++).type, p);
                p = next_p;
                return member;
            });

        return {
            llvm::ConstantStruct::get(
                llvm::cast<llvm::StructType>(llvm_type),
                members),
            p
        };
    }

    return d_context.getLLVMConstant(t.simpletype(), p);
}

llvm::Constant *
Shader::IRGenContext::getLLVMConstant(const Symbol& s) {
    if (s.data()) {
        return getLLVMConstant(s.typespec(), s.data()).first;
    }

    return getLLVMDefaultConstant(s.typespec());
}

void
Shader::IRGenContext::beginFunction(
    llvm::StringRef name, llvm::Type *return_type, llvm::ArrayRef<llvm::Type *> param_types) {
    assert(d_state == State::Initial);

    d_function.reset(llvm::Function::Create(
        llvm::FunctionType::get(
            return_type, param_types,
            false),
        llvm::GlobalValue::ExternalLinkage, name, &d_module));

    d_state = State::Function;
}

std::unique_ptr<llvm::Function>
Shader::IRGenContext::endFunction() {
    assert(d_state == State::Function);

    // Order the blocks topologically:
    struct Frame {
        llvm::BasicBlock *block = nullptr;
        bool back = false;
    };

    std::stack<Frame> stack;

    enum class Color {
        White, Grey, Black
    };

    std::list<llvm::BasicBlock *> blocks;
    llvm::DenseMap<llvm::BasicBlock *, Color> color;

    std::for_each(
        d_function->begin(), d_function->end(),
        [&blocks, &color](auto& block) -> void {
            blocks.push_back(&block);
            color[&block] = Color::White;
        });

    llvm::BasicBlock *insertion_point = nullptr;

    for (auto block : blocks) {
        if (color[block] != Color::White) {
            continue;
        }

        stack.push({ block, false });

        while (!stack.empty()) {
            auto [ block, back ] = stack.top();
            stack.pop();

            if (!back) {
                color[block] = Color::Grey;

                stack.push({ block, true });

                std::for_each(
                    llvm::pred_begin(block), llvm::pred_end(block),
                    [&stack, &color](auto pred) -> void {
                        if (color[pred] != Color::White) {
                            return;
                        }

                        stack.push({ pred, false });
                    });
            }
            else {
                color[block] = Color::Black;

                if (!insertion_point) {
                    block->moveBefore(&d_function->front());
                }
                else {
                    block->moveAfter(insertion_point);
                }

                insertion_point = block;
            }
        }
    }

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

llvm::Value *
Shader::IRGenContext::getSymbolAddress(const Symbol& symbol) {
    assert(d_symbol_addresses.count(&symbol) > 0);
    return d_symbol_addresses[&symbol];
}

llvm::Value *
Shader::IRGenContext::getSymbolValue(llvm::IRBuilder<>& builder, const Symbol& symbol) {
    if (d_symbol_values.count(&symbol) > 0) {
        return d_symbol_values[&symbol];
    }

    return builder.CreateLoad(getSymbolAddress(symbol));
}

llvm::Value *
Shader::IRGenContext::getSymbolConditionValue(llvm::IRBuilder<>& builder, const Symbol& symbol) {
    return builder.CreateTrunc(
        getSymbolValue(builder, symbol),
        llvm::Type::getInt1Ty(d_ll_context));
}

llvm::Value *
Shader::IRGenContext::convertValue(llvm::IRBuilder<>& builder, const OSL::TypeDesc& lhs_type, const OSL::TypeDesc& rhs_type, llvm::Value *rhs_value) {
    const auto& lhs_traits = OSLScalarTypeTraits::get(lhs_type);
    const auto& rhs_traits = OSLScalarTypeTraits::get(rhs_type);

    auto lhs_llvm_basetype = d_context.getLLVMType(
        OSL::TypeDesc((OSL::TypeDesc::BASETYPE)lhs_type.basetype));

    // int-to-int
    if (lhs_traits.type == OSLScalarTypeTraits::Integer && rhs_traits.type == OSLScalarTypeTraits::Integer) {
        if (lhs_traits.width < rhs_traits.width) {
            rhs_value = builder.CreateTrunc(rhs_value, lhs_llvm_basetype);
        }
        else if (lhs_traits.width > rhs_traits.width) {
            if (lhs_traits.sign) {
                rhs_value = builder.CreateSExt(rhs_value, lhs_llvm_basetype);
            }
            else {
                rhs_value = builder.CreateZExt(rhs_value, lhs_llvm_basetype);
            }
        }
    }
    // int-to-fp
    else if (lhs_traits.type == OSLScalarTypeTraits::Real && rhs_traits.type == OSLScalarTypeTraits::Integer) {
        if (rhs_traits.sign) {
            rhs_value = builder.CreateSIToFP(rhs_value, lhs_llvm_basetype);
        }
        else {
            rhs_value = builder.CreateUIToFP(rhs_value, lhs_llvm_basetype);
        }
    }
    // fp-to-int
    else if (lhs_traits.type == OSLScalarTypeTraits::Integer && rhs_traits.type == OSLScalarTypeTraits::Real) {
        if (lhs_traits.sign) {
            rhs_value = builder.CreateFPToSI(rhs_value, lhs_llvm_basetype);
        }
        else {
            rhs_value = builder.CreateFPToUI(rhs_value, lhs_llvm_basetype);
        }
    }
    // fp-to-fp
    else if (lhs_traits.type == OSLScalarTypeTraits::Real && rhs_traits.type == OSLScalarTypeTraits::Real) {
        if (lhs_traits.width < rhs_traits.width) {
            rhs_value = builder.CreateFPTrunc(rhs_value, lhs_llvm_basetype);
        }
        else if (lhs_traits.width > rhs_traits.width) {
            rhs_value = builder.CreateFPExt(rhs_value, lhs_llvm_basetype);
        }
    }

    //
    auto lhs_aggregate = lhs_type.aggregate;
    auto rhs_aggregate = rhs_type.aggregate;

    auto lhs_llvm_aggtype = d_context.getLLVMType(
        OSL::TypeDesc((OSL::TypeDesc::BASETYPE)lhs_type.basetype,
                      (OSL::TypeDesc::AGGREGATE)lhs_type.aggregate));

    if (lhs_aggregate > 1 && rhs_aggregate == 1) {
        if (lhs_aggregate >= 2 && lhs_aggregate <= 4) {
            llvm::Value *tmp = llvm::UndefValue::get(lhs_llvm_aggtype);

            for (unsigned i = 0, n = (unsigned)lhs_aggregate; i < n; ++i) {
                tmp = builder.CreateInsertElement(tmp, rhs_value, i);
            }

            rhs_value = tmp;
        }
    }

    return rhs_value;
}

llvm::BasicBlock *
Shader::IRGenContext::createBlock(llvm::StringRef name) {
    assert(d_state == State::Function || d_state == State::Block);

    return llvm::BasicBlock::Create(d_ll_context, name, d_function.get());
}

void
Shader::IRGenContext::beginBlock(llvm::BasicBlock *block) {
    assert(d_state == State::Function);

    d_block = block;
    d_builder.reset(new llvm::IRBuilder<>(d_block));

    d_state = State::Block;
}

void
Shader::IRGenContext::continueBlock(llvm::BasicBlock *block) {
    assert(d_state == State::Block);

    d_block = block;
    d_builder->SetInsertPoint(d_block);
}

void
Shader::IRGenContext::endBlock() {
    assert(d_state == State::Block);

    d_block = nullptr;
    d_builder.reset();

    d_state = State::Function;
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

    d_module = std::make_unique<llvm::Module>(shader_master.shadername(), ll_context);

    IRGenContext irgen_context(*d_context, *d_module);

    const auto& symbols = shader_master.symbols();

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
    std::vector<llvm::Type *> input_types, return_types;
    input_types.reserve(input_count + 1);
    return_types.reserve(output_count);

    input_types.push_back(
        llvm::PointerType::get(
            irgen_context.getShaderGlobalsType(), 0));

    std::for_each(
        symbols.begin(), symbols.end(),
        [&irgen_context, &input_types, &return_types](const auto& symbol) -> void {
            auto s = symbol.dealias();

            switch (s->symtype()) {
                case SymTypeParam: {
                    // Some inputs are passed by reference:
                    const auto& t = s->typespec();

                    if (!t.is_closure() && (t.is_matrix() || t.is_structure() || t.is_sized_array() || t.is_string())) {
                        input_types.push_back(
                            llvm::PointerType::get(
                                irgen_context.getLLVMType(t), 0));
                    }
                    else {
                        input_types.push_back(irgen_context.getLLVMType(t));
                    }

                } return;
                case SymTypeOutputParam: {
                    return_types.push_back(irgen_context.getLLVMType(s->typespec()));
                } return;
                default:
                    break;
            }
        });

    llvm::Type *return_type =
        (output_count > 0)
        ? llvm::StructType::get(ll_context, return_types)
        : llvm::Type::getVoidTy(ll_context);

    irgen_context.beginFunction(
        shader_master.shadername(), return_type, input_types);

    // Create the 'entry' block:
    auto entry_block = irgen_context.createBlock("entry");
    irgen_context.beginBlock(entry_block);

    // Allocate space for the result value:
    llvm::Value *result =
        (output_count > 0)
        ? irgen_context.builder().CreateAlloca(return_type, nullptr, "result")
        : nullptr;

    // Initialize symbols:
    auto input_it = irgen_context.function()->arg_begin();

    auto shaderglobals = input_it++;
    shaderglobals->setName("shaderglobals");

    unsigned output_index = 0;

    std::for_each(
        symbols.begin(), symbols.end(),
        [&irgen_context,
         shaderglobals, &input_it, result, &output_index](const auto& symbol) -> void {
            auto s = symbol.dealias();
            if (!s->everused()) {
                return;
            }

            auto name = s->name().string();
            const auto& t = s->typespec();

            switch (s->symtype()) {
                case SymTypeGlobal: {
                    const auto& index = irgen_context.getShaderGlobalsIndex();
                    auto it = index.find(s->name());
                    assert(it != index.end());

                    auto address = irgen_context.builder().CreateStructGEP(nullptr, shaderglobals, it->second);

                    if (s->everwritten() ||
                        t.is_matrix() || t.is_structure() || t.is_sized_array() || t.is_string()) {
                        address->setName(name);
                        irgen_context.insertSymbolAddress(*s, address);
                    }
                    else {
                        auto value = irgen_context.builder().CreateLoad(address, name);
                        irgen_context.insertSymbolValue(*s, value);
                    }
                } break;
                case SymTypeParam: {
                    auto value = input_it++;
                    value->setName(name);

                    // Some inputs are passed by reference:
                    if (!t.is_closure() && (t.is_matrix() || t.is_structure() || t.is_sized_array() || t.is_string())) {
                        irgen_context.insertSymbolAddress(*s, value);
                    }
                    else {
                        irgen_context.insertSymbolValue(*s, value);
                    }
                } break;
                case SymTypeOutputParam: {
                    auto value = irgen_context.builder().CreateStructGEP(nullptr, result, output_index++, name);
                    irgen_context.insertSymbolAddress(*s, value);
                } break;
                case SymTypeLocal:
                case SymTypeTemp: {
                    // Locals need both an address and an initial value:
                    auto address = irgen_context.builder().CreateAlloca(irgen_context.getLLVMType(s->typespec()), nullptr, name);
                    auto value = irgen_context.getLLVMConstant(*s);
                    assert(value);

                    irgen_context.builder().CreateStore(value, address);
                    irgen_context.insertSymbolAddress(*s, address);
                } break;
                case SymTypeConst: {
                    // Constants are inlined:
                    auto value = irgen_context.getLLVMConstant(*s);
                    assert(value);

                    // Some constants will be passed on by reference:
                    if (t.is_matrix() || t.is_structure() || t.is_sized_array() || t.is_string()) {
                        auto address = irgen_context.builder().CreateAlloca(irgen_context.getLLVMType(s->typespec()), nullptr, name);
                        irgen_context.builder().CreateStore(value, address);
                        irgen_context.insertSymbolAddress(*s, address);
                    }
                    else {
                        irgen_context.insertSymbolValue(*s, value);
                    }
                } break;
                default:
                    break;
            }
        });

    auto body_block = irgen_context.createBlock();
    auto branch = irgen_context.builder().CreateBr(body_block);
    irgen_context.endBlock();

    // Create the 'exit' block:
    auto exit_block = irgen_context.createBlock("exit");
    irgen_context.beginBlock(exit_block);

    if (output_count > 0) {
        irgen_context.builder().CreateRet(
            irgen_context.builder().CreateLoad(result));
    }
    else {
        irgen_context.builder().CreateRetVoid();
    }

    irgen_context.endBlock();

    //
    struct Frame {
        std::string name;
        unsigned block_id;
        llvm::BasicBlock *block = nullptr;
        llvm::BasicBlock *merge_block = nullptr;
        llvm::BasicBlock *continue_block = nullptr, *break_block = nullptr;
        int opcode_index = 0, opcode_end = 0;
    };

    std::stack<Frame> stack;

    const auto& ops  = shader_master.ops();
    const auto& args = shader_master.args();

    stack.push({
        "body", 0,
        body_block, exit_block, nullptr, nullptr,
        shader_master.maincodebegin(), shader_master.maincodeend()
    });

    unsigned flow_id = 0;
    std::unordered_map<std::string, unsigned> function_id;

    while (!stack.empty()) {
        auto [ name, block_id, block, merge_block, continue_block, break_block, opcode_index, opcode_end ] = stack.top();
        stack.pop();

        if (block_id == 0) {
            block->setName(name);
            ++block_id;
        }
        else {
            block->setName(llvm::formatv("{0}.{1}", name, block_id++).str());
        }

        irgen_context.beginBlock(block);

        while (opcode_index < opcode_end) {
            const auto& opcode = ops[opcode_index++];
            const auto& opname = opcode.opname();

            std::cerr << opname.string() << std::endl;

            if (opname == Ops::u_end) {
                break;
            }

            if (opname == Ops::u_nop) {
                continue;
            }

            std::vector<const Symbol *> opargs;
            opargs.reserve(opcode.nargs());
            for (int i = opcode.firstarg(), n = opcode.nargs(); n > 0; ++i, --n) {
                auto opsym = symbols[args[i]].dealias();
                std::cerr << "\t" << opsym->name().string() << " " << opsym->typespec().string() << std::endl;

                opargs.push_back(symbols[args[i]].dealias());
            }

            if (opname == Ops::u_functioncall) {
                auto function_name = opargs[0]->get_string().string();
                std::cerr << "\t" << function_name << std::endl;

                auto it = function_id.insert({ function_name, 0 }).first;
                function_name = llvm::formatv("call.{0}.{1}", function_name, it->second++);

                auto function_block = irgen_context.createBlock();

                block = irgen_context.createBlock();
                block->setName(llvm::formatv("{0}.{1}", name, block_id++).str());

                auto function_opcode_index = std::exchange(opcode_index, opcode.jump(0));
                auto function_opcode_end = opcode_index;

                stack.push({
                    function_name, 0, function_block, block, nullptr, nullptr, function_opcode_index, function_opcode_end
                });

                irgen_context.builder().CreateBr(function_block);
                irgen_context.continueBlock(block);

                continue;
            }

            if (opname == Ops::u_return) {
                break;
            }

            if (opname == Ops::u_if) {
                auto if_name = llvm::formatv("if.{0}", flow_id++).str();

                llvm::BasicBlock *then_block = nullptr, *else_block = nullptr;

                block = irgen_context.createBlock();
                block->setName(llvm::formatv("{0}.{1}", name, block_id++).str());

                // 'then' clause:
                if (opcode.jump(0) >= opcode_index) {
                    then_block = irgen_context.createBlock();

                    auto begin = std::exchange(opcode_index, opcode.jump(0));
                    auto end = opcode_index;

                    stack.push({
                        if_name + ".then", 0, then_block, block, continue_block, break_block, begin, end
                    });
                }

                // 'else' clause:
                if (opcode.jump(1) > opcode.jump(0)) {
                    else_block = irgen_context.createBlock();

                    auto begin = std::exchange(opcode_index, opcode.jump(1));
                    auto end = opcode_index;

                    stack.push({
                        if_name + ".else", 0, else_block, block, continue_block, break_block, begin, end
                    });
                }

                auto value = irgen_context.getSymbolConditionValue(*opargs[0]);
                irgen_context.builder().CreateCondBr(value, then_block, else_block ? else_block : block);
                irgen_context.continueBlock(block);
            }

            if (opname == Ops::u_for || opname == Ops::u_while || opname == Ops::u_dowhile) {
                std::string loop_name;

                if (opname == Ops::u_for) {
                    loop_name = llvm::formatv("for.{0}", flow_id++);
                }
                else if (opname == Ops::u_while) {
                    loop_name = llvm::formatv("while.{0}", flow_id++);
                }
                else if (opname == Ops::u_dowhile) {
                    loop_name = llvm::formatv("dowhile.{0}", flow_id++);
                }

                auto this_flow_id = flow_id++;

                // pred_block; always needed:
                auto pred_block = irgen_context.createBlock();

                // cond_block: jump(0) to jump(1)
                auto cond_block = (opcode.jump(1) > opcode.jump(0)) ? irgen_context.createBlock()
                                                                    : pred_block;

                // body_block: jump(1) to jump(2)
                llvm::BasicBlock *body_block = (opcode.jump(2) > opcode.jump(1)) ? irgen_context.createBlock()
                                                                                 : nullptr;

                // ind_block: jump(2) to jump(3)
                llvm::BasicBlock *ind_block = (opcode.jump(3) > opcode.jump(2)) ? irgen_context.createBlock()
                                                                                : nullptr;

                // next_block: jump(2) or jump(3) to opcode_end
                llvm::BasicBlock *exit_block = irgen_context.createBlock();

                //
                llvm::BasicBlock *iter_entry = nullptr,
                                 *pred_exit  = nullptr,
                                 *body_exit  = nullptr;

                if (opname == Ops::u_for || opname == Ops::u_while) {
                    // cond->pred->body->ind->cond
                    //         +-->exit
                    iter_entry = cond_block;
                    pred_exit  = body_block;
                    body_exit  = ind_block ? ind_block : cond_block;
                }
                else if (opname == Ops::u_dowhile) {
                    // body->cond->pred->ind->body
                    //               +-->exit
                    iter_entry = body_block;
                    pred_exit  = ind_block ? ind_block : body_block;
                    body_exit  = cond_block;
                }

                // populate pred_block:
                {   pred_block->setName(loop_name + ".pred");

                    assert(pred_exit);
                    auto& builder = irgen_context.builder();

                    llvm::IRBuilder<>::InsertPointGuard guard(builder);
                    builder.SetInsertPoint(pred_block);

                    auto value = irgen_context.getSymbolConditionValue(*opargs[0]);
                    builder.CreateCondBr(value, pred_exit, exit_block);
                }

                // populate cond_block:
                if (cond_block != pred_block) {
                    stack.push({
                        loop_name + ".cond", 0, cond_block, pred_block, nullptr, nullptr, opcode.jump(0), opcode.jump(1)
                    });
                }

                // populate body_block:
                if (body_block) {
                    assert(body_exit);
                    stack.push({
                        loop_name + ".body", 0, body_block, body_exit, body_exit, exit_block, opcode.jump(1), opcode.jump(2)
                    });
                }

                // populate ind_block:
                if (ind_block) {
                    assert(iter_entry);
                    stack.push({
                        loop_name + ".ind", 0, ind_block, iter_entry, nullptr, nullptr, opcode.jump(2), opcode.jump(3)
                    });
                }

                // populate exit_block:
                stack.push({
                    name, block_id++, exit_block, merge_block, continue_block, break_block, opcode.jump(3), opcode_end
                });

                opcode_end = opcode.jump(0);

                assert(iter_entry);
                merge_block = iter_entry;

                continue;
            }

            if (opname == Ops::u_continue) {
                assert(continue_block);

                block = irgen_context.createBlock();
                block->setName(llvm::formatv("{0}.{1}", name, block_id++).str());

                irgen_context.builder().CreateBr(continue_block);
                irgen_context.continueBlock(block);

                continue;
            }

            if (opname == Ops::u_break) {
                assert(break_block);

                block = irgen_context.createBlock();

                irgen_context.builder().CreateBr(break_block);
                irgen_context.continueBlock(block);

                continue;
            }

            if (opname == Ops::u_assign) {
                const auto& ltype = opargs[0]->typespec();
                const auto& rtype = opargs[1]->typespec();

                auto lvalue = irgen_context.getSymbolAddress(*opargs[0]);
                auto rvalue = irgen_context.getSymbolValue(*opargs[1]);

                if (ltype.is_closure() || ltype.is_matrix() || ltype.is_structure() || ltype.is_string()) {
                    if (ltype.is_closure() && rtype.is_int()) {
                        assert(llvm::isa<llvm::ConstantInt>(rvalue));
                        assert(llvm::cast<llvm::ConstantInt>(rvalue)->isZero());
                        irgen_context.builder().CreateStore(
                            llvm::ConstantPointerNull::get(irgen_context.getClosureType()),
                            lvalue);
                        continue;
                    }

                    if (ltype.is_closure()) {
                        assert(rtype.is_closure());
                    }
                    else if (ltype.is_matrix()) {
                        assert(rtype.is_matrix());
                    }
                    else if (ltype.is_structure()) {
                        assert(rtype.is_structure());
                    }
                    else if (ltype.is_string()) {
                        assert(rtype.is_string());
                    }

                    irgen_context.builder().CreateStore(rvalue, lvalue);

                    continue;
                }

                rvalue = irgen_context.convertValue(ltype.simpletype(),
                                                    rtype.simpletype(),
                                                    rvalue);

                irgen_context.builder().CreateStore(rvalue, lvalue);

                continue;
            }

            if (opname == Ops::u_add || opname == Ops::u_sub || opname == Ops::u_mul || opname == Ops::u_div) {
                const auto& ltype  = opargs[0]->typespec();
                const auto& rtype0 = opargs[1]->typespec();
                const auto& rtype1 = opargs[2]->typespec();

                auto lvalue  = irgen_context.getSymbolAddress(*opargs[0]);
                auto rvalue0 = irgen_context.getSymbolValue(*opargs[1]);
                auto rvalue1 = irgen_context.getSymbolValue(*opargs[2]);

                if (ltype.is_closure()) {
                    // TODO
                    continue;
                }

                rvalue0 = irgen_context.convertValue(ltype.simpletype(),
                                                     rtype0.simpletype(),
                                                     rvalue0);
                rvalue1 = irgen_context.convertValue(ltype.simpletype(),
                                                     rtype1.simpletype(),
                                                     rvalue1);

                const auto [ type, sign, width ] = OSLScalarTypeTraits::get(ltype.simpletype());
                llvm::Value *result = nullptr;

                if (type == OSLScalarTypeTraits::Integer) {
                    if (opname == Ops::u_add) {
                        result = irgen_context.builder().CreateAdd(rvalue0, rvalue1);
                    }
                    else if (opname == Ops::u_sub) {
                        result = irgen_context.builder().CreateSub(rvalue0, rvalue1);
                    }
                    else if (opname == Ops::u_mul) {
                        result = irgen_context.builder().CreateMul(rvalue0, rvalue1);
                    }
                    else if (opname == Ops::u_div) {
                        if (sign) {
                            result = irgen_context.builder().CreateSDiv(rvalue0, rvalue1);
                        }
                        else {
                            result = irgen_context.builder().CreateUDiv(rvalue0, rvalue1);
                        }
                    }
                    else if (opname == Ops::u_mod) {
                        if (sign) {
                            result = irgen_context.builder().CreateSRem(rvalue0, rvalue1);
                        }
                        else {
                            result = irgen_context.builder().CreateURem(rvalue0, rvalue1);
                        }
                    }

                    assert(result);
                }
                else if (type == OSLScalarTypeTraits::Real) {
                    if (opname == Ops::u_add) {
                        result = irgen_context.builder().CreateFAdd(rvalue0, rvalue1);
                    }
                    else if (opname == Ops::u_sub) {
                        result = irgen_context.builder().CreateFSub(rvalue0, rvalue1);
                    }
                    else if (opname == Ops::u_mul) {
                        result = irgen_context.builder().CreateFMul(rvalue0, rvalue1);
                    }
                    else if (opname == Ops::u_div) {
                        result = irgen_context.builder().CreateFDiv(rvalue0, rvalue1);
                    }
                    else if (opname == Ops::u_mod) {
                        result = irgen_context.builder().CreateFRem(rvalue0, rvalue1);
                    }

                    assert(result);
                }

                irgen_context.builder().CreateStore(result, lvalue);

                continue;
            }

            if (opname == Ops::u_neg) {
                const auto& ltype = opargs[0]->typespec();
                const auto& rtype = opargs[1]->typespec();

                auto lvalue = irgen_context.getSymbolAddress(*opargs[0]);
                auto rvalue = irgen_context.getSymbolValue(*opargs[1]);

                rvalue = irgen_context.convertValue(ltype.simpletype(), rtype.simpletype(), rvalue);

                const auto [ type, sign, width ] = OSLScalarTypeTraits::get(ltype.simpletype());
                llvm::Value *result = nullptr;

                if (type == OSLScalarTypeTraits::Integer) {
                    result = irgen_context.builder().CreateNeg(rvalue);
                }
                else if (type == OSLScalarTypeTraits::Real) {
                    result = irgen_context.builder().CreateFNeg(rvalue);
                }

                irgen_context.builder().CreateStore(result, lvalue);

                continue;
            }

            if (opname == Ops::u_eq || opname == Ops::u_neq || opname == Ops::u_lt || opname == Ops::u_gt || opname == Ops::u_le || opname == Ops::u_ge) {
                const auto& ltype  = opargs[0]->typespec();
                const auto& rtype0 = opargs[1]->typespec();
                const auto& rtype1 = opargs[2]->typespec();

                auto lvalue = irgen_context.getSymbolAddress(*opargs[0]);
                auto rvalue0 = irgen_context.getSymbolValue(*opargs[1]);
                auto rvalue1 = irgen_context.getSymbolValue(*opargs[2]);

                llvm::Value *result = nullptr;

                if (rtype0.is_closure() || rtype1.is_closure()) {
                    assert((rtype0.is_closure() && rtype1.is_int()) ||
                           (rtype1.is_closure() && rtype0.is_int()));
                    assert((rtype0.is_closure() && llvm::isa<llvm::ConstantInt>(rvalue1) && llvm::cast<llvm::ConstantInt>(rvalue1)->isZero()) ||
                           (rtype1.is_closure() && llvm::isa<llvm::ConstantInt>(rvalue0) && llvm::cast<llvm::ConstantInt>(rvalue0)->isZero()));
                    assert(opname == Ops::u_eq || opname == Ops::u_neq);

                    auto closure = rtype0.is_closure() ? rvalue0 : rvalue1;

                    if (opname == Ops::u_eq) {
                        result = irgen_context.builder().CreateICmpEQ(
                            closure,
                            llvm::ConstantPointerNull::get(irgen_context.getClosureType()));
                    }
                    else if (opname == Ops::u_neq) {
                        result = irgen_context.builder().CreateICmpNE(
                            closure,
                            llvm::ConstantPointerNull::get(irgen_context.getClosureType()));
                    }
                }
                else if (rtype0.is_string() || rtype1.is_string()) {
                    assert(rtype0.is_string() == rtype1.is_string());
                    // TODO
                    continue;
                }
                else {
                    auto ptype = getPromotionType(std::vector<OSL::TypeDesc>{ rtype0.simpletype(),
                                                                              rtype1.simpletype() });

                    rvalue0 = irgen_context.convertValue(ptype, rtype0.simpletype(), rvalue0);
                    rvalue1 = irgen_context.convertValue(ptype, rtype1.simpletype(), rvalue1);

                    const auto [ type, sign, width ] = OSLScalarTypeTraits::get(ptype);

                    if (type == OSLScalarTypeTraits::Integer) {
                        if (opname == Ops::u_eq) {
                            result = irgen_context.builder().CreateICmpEQ(rvalue0, rvalue1);
                        }
                        else if (opname == Ops::u_neq) {
                            result = irgen_context.builder().CreateICmpNE(rvalue0, rvalue1);
                        }
                        else if (opname == Ops::u_lt) {
                            if (sign) {
                                result = irgen_context.builder().CreateICmpSLT(rvalue0, rvalue1);
                            }
                            else {
                                result = irgen_context.builder().CreateICmpULT(rvalue0, rvalue1);
                            }
                        }
                        else if (opname == Ops::u_gt) {
                            if (sign) {
                                result = irgen_context.builder().CreateICmpSGT(rvalue0, rvalue1);
                            }
                            else {
                                result = irgen_context.builder().CreateICmpUGT(rvalue0, rvalue1);
                            }
                        }
                        else if (opname == Ops::u_le) {
                            if (sign) {
                                result = irgen_context.builder().CreateICmpSLE(rvalue0, rvalue1);
                            }
                            else {
                                result = irgen_context.builder().CreateICmpULE(rvalue0, rvalue1);
                            }
                        }
                        else if (opname == Ops::u_ge) {
                            if (sign) {
                                result = irgen_context.builder().CreateICmpSGE(rvalue0, rvalue1);
                            }
                            else {
                                result = irgen_context.builder().CreateICmpUGE(rvalue0, rvalue1);
                            }
                        }
                    }
                    else if (type == OSLScalarTypeTraits::Real) {
                        if (opname == Ops::u_eq) {
                            result = irgen_context.builder().CreateFCmpUEQ(rvalue0, rvalue1);
                        }
                        else if (opname == Ops::u_neq) {
                            result = irgen_context.builder().CreateFCmpUNE(rvalue0, rvalue1);
                        }
                        else if (opname == Ops::u_lt) {
                            result = irgen_context.builder().CreateFCmpULT(rvalue0, rvalue1);
                        }
                        else if (opname == Ops::u_gt) {
                            result = irgen_context.builder().CreateFCmpUGT(rvalue0, rvalue1);
                        }
                        else if (opname == Ops::u_le) {
                            result = irgen_context.builder().CreateFCmpULE(rvalue0, rvalue1);
                        }
                        else if (opname == Ops::u_ge) {
                            result = irgen_context.builder().CreateFCmpUGE(rvalue0, rvalue1);
                        }
                    }

                    if (ptype.aggregate != OSL::TypeDesc::SCALAR) {
                        result = irgen_context.builder().CreateAndReduce(result);
                    }
                }

                assert(result);

                result = irgen_context.builder().CreateZExt(
                    result,
                    d_context->getLLVMType(
                        OSL::TypeDesc((OSL::TypeDesc::BASETYPE)ltype.simpletype().basetype)));

                irgen_context.builder().CreateStore(result, lvalue);

                continue;
            }

            if (opname == Ops::u_bitand || opname == Ops::u_bitor || opname == Ops::u_xor || opname == Ops::u_shl || opname == Ops::u_shr) {
                const auto& ltype  = opargs[0]->typespec();
                const auto& rtype0 = opargs[1]->typespec();
                const auto& rtype1 = opargs[2]->typespec();

                auto lvalue  = irgen_context.getSymbolAddress(*opargs[0]);
                auto rvalue0 = irgen_context.getSymbolValue(*opargs[1]);
                auto rvalue1 = irgen_context.getSymbolValue(*opargs[2]);

                rvalue0 = irgen_context.convertValue(ltype.simpletype(),
                                                     rtype0.simpletype(),
                                                     rvalue0);
                rvalue1 = irgen_context.convertValue(ltype.simpletype(),
                                                     rtype1.simpletype(),
                                                     rvalue1);

                llvm::Value *result = nullptr;

                if (opname == Ops::u_bitand) {
                    result = irgen_context.builder().CreateAnd(rvalue0, rvalue1);
                }
                else if (opname == Ops::u_bitor) {
                    result = irgen_context.builder().CreateOr(rvalue0, rvalue1);
                }
                else if (opname == Ops::u_xor) {
                    result = irgen_context.builder().CreateXor(rvalue0, rvalue1);
                }
                else if (opname == Ops::u_shl) {
                    result = irgen_context.builder().CreateShl(rvalue0, rvalue1);
                }
                else if (opname == Ops::u_shr) {
                    result = irgen_context.builder().CreateAShr(rvalue0, rvalue1);
                }

                irgen_context.builder().CreateStore(result, lvalue);

                continue;
            }

            if (opname == Ops::u_compl) {
                const auto& ltype = opargs[0]->typespec();
                const auto& rtype = opargs[1]->typespec();

                auto lvalue = irgen_context.getSymbolAddress(*opargs[0]);
                auto rvalue = irgen_context.getSymbolValue(*opargs[1]);

                rvalue = irgen_context.convertValue(ltype.simpletype(), rtype.simpletype(), rvalue);

                irgen_context.builder().CreateStore(
                    irgen_context.builder().CreateNeg(rvalue),
                    lvalue);

                continue;
            }
        }

        irgen_context.builder().CreateBr(merge_block);

        irgen_context.endBlock();
    }

    auto function = irgen_context.endFunction();

    d_main_function_md.reset(
        llvm::ValueAsMetadata::get(function.release()));

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
