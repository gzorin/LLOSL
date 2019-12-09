#include "BuilderImpl.h"
#include "Library.h"
#include "LLOSLContextImpl.h"
#include "StringUtil.h"
#include "SymbolScope.h"
#include "TypeScope.h"

#include <llosl/IR/BXDFAST.h>
#include <llosl/IR/BXDFPass.h>
#include <llosl/IR/ClosureIRPass.h>
#include <llosl/IR/InstrumentationPass.h>
#include <llosl/IR/PathInfoPass.h>
#include <llosl/Closure.h>
#include <llosl/Shader.h>

#include <llvm/ADT/APInt.h>
#include <llvm/IR/Argument.h>
#include <llvm/IR/BasicBlock.h>
#include <llvm/IR/CFG.h>
#include <llvm/IR/DerivedTypes.h>
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/Instruction.h>
#include <llvm/IR/LegacyPassManager.h>
#include <llvm/IR/Metadata.h>
#include <llvm/IR/Module.h>
#include <llvm/IR/Type.h>
#include <llvm/Support/FormatVariadic.h>
#include <llvm/Transforms/IPO.h>
#include <llvm/Transforms/Scalar.h>

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
ustring u_closure("closure");

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

void
SortBasicBlocksTopologically(llvm::Function *function) {
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
        function->begin(), function->end(),
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
                    block->moveBefore(&function->front());
                }
                else {
                    block->moveAfter(insertion_point);
                }

                insertion_point = block;
            }
        }
    }
}

llvm::Value *
CreateCastToCondition(llvm::IRBuilder<>& builder, llvm::Value *value) {
    return builder.CreateTrunc(value, builder.getInt1Ty());
}

llvm::Value *
CreateCast(TypeScope& type_scope, llvm::IRBuilder<>& builder,
           const OSL::TypeDesc& rhs_type, llvm::Value *rhs_value,
           const OSL::TypeDesc& lhs_type) {
    const auto& lhs_traits = OSLScalarTypeTraits::get(lhs_type);
    const auto& rhs_traits = OSLScalarTypeTraits::get(rhs_type);

    auto lhs_llvm_basetype = type_scope.get(
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

    auto lhs_llvm_aggtype = type_scope.get(
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

Shader::Shader(LLOSLContextImpl& context, OSL::ShaderGroup& shader_group)
    : d_context(&context) {
    auto& ll_context = d_context->getLLContext();

    d_context->addShader(this);

    OSL::pvt::RuntimeOptimizer rop(d_context->getShadingContext()->shadingsys(), shader_group, nullptr);
    rop.run();

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

Shader::Shader(LLOSLContextImpl& context, OSL::pvt::ShaderMaster& shader_master)
    : d_context(&context) {
    auto& ll_context = d_context->getLLContext();

    d_context->addShader(this);

    TypeScope type_scope(context);

    //
    d_module = std::make_unique<llvm::Module>(shader_master.shadername(), ll_context);

    //
    Library runtime_library(context, type_scope, *d_module);

    runtime_library.declare("llosl_closure_output_annotation", "xC");
    runtime_library.declare("llosl_closure_storage_annotation", "xC");

    #define DECL(name,signature) runtime_library.declare(#name, signature);
    #include "builtindecl.h"
    #undef DECL

    //
    llvm::IRBuilder<> builder(ll_context);

    //
    d_parameter_count = shader_master.num_params();
    d_parameters = std::allocator<Parameter>().allocate(d_parameter_count);

    auto parameter = d_parameters;
    unsigned parameter_index = 0;

    std::vector<llvm::Type *> param_types;
    param_types.reserve(d_parameter_count + 1);

    auto it_param_type = std::back_inserter(param_types);

    *it_param_type++ = llvm::PointerType::get(d_context->getShaderGlobalsType(), 0);

    const auto& symbols = shader_master.symbols();
    std::for_each(
        symbols.begin(), symbols.end(),
        [this, &type_scope, &parameter, &parameter_index, &it_param_type](const auto& symbol) -> void {
            auto s = symbol.dealias();
            auto st = s->symtype();

            if (st != SymTypeParam && st != SymTypeOutputParam) {
                return;
            }

            const auto&t = s->typespec();
            llvm::Type *type = nullptr;

            if (st == SymTypeParam) {
                type = type_scope.getForArgument(t);
            }
            else if (st == SymTypeOutputParam) {
                if (type_scope.isPassedByReference(t)) {
                    type = type_scope.getForArgument(t);
                }
                else {
                    type = llvm::PointerType::get(
                        type_scope.get(t), 0);
                }
            }

            new (parameter++) Parameter(
                st == SymTypeOutputParam,
                type, parameter_index++, { s->name() }, t.is_closure(), t.simpletype(), this);

            *it_param_type++ = type;
        });

    auto function_name = shader_master.shadername();

    auto function = llvm::Function::Create(
        llvm::FunctionType::get(
            llvm::Type::getVoidTy(ll_context),
            param_types,
            false),
        llvm::GlobalValue::ExternalLinkage, function_name, d_module.get());

    auto shader_globals = function->arg_begin();
    shader_globals->setName("shaderglobals");

    std::accumulate(
        symbols.begin(), symbols.end(),
        function->arg_begin() + 1,
        [](auto it_arg, const auto& symbol) -> auto {
            auto s = symbol.dealias();
            auto st = s->symtype();

            if (st == SymTypeParam || st == SymTypeOutputParam) {
                it_arg++->setName(makeLLVMStringRef(s->name()));
            }

            return it_arg;
        });

    SymbolScope symbol_scope(context, type_scope, runtime_library, builder,
                             function, shader_globals);

    Library::CallingContext runtime_library_context(runtime_library, symbol_scope, builder);

    // Create the 'entry' block:
    auto entry_block = llvm::BasicBlock::Create(ll_context, "entry", function);
    builder.SetInsertPoint(entry_block);

    auto renderer = builder.CreateLoad(
        builder.CreateStructGEP(
            nullptr, shader_globals,
            getShaderGlobalsIndex().find(OSL::ustring("renderer"))->second));
    renderer->setName("renderer");

    std::for_each(
        symbols.begin(), symbols.end(),
        [&symbol_scope](const auto& s) -> void {
            symbol_scope.add(&s);
        });

    // Create the 'body' block, and branch to it:
    auto body_block = llvm::BasicBlock::Create(ll_context, "body", function);
    builder.CreateBr(body_block);

    builder.ClearInsertionPoint();

    // Create the 'exit' block, and populate it with a return instruction:
    auto exit_block = llvm::BasicBlock::Create(ll_context, "exit", function);
    builder.SetInsertPoint(exit_block);
    builder.CreateRetVoid();
    builder.ClearInsertionPoint();

    //
    auto osl_allocate_closure_component          = runtime_library_context["osl_allocate_closure_component"];
    auto osl_allocate_weighted_closure_component = runtime_library_context["osl_allocate_weighted_closure_component"];
    auto osl_add_closure_closure                 = runtime_library_context["osl_add_closure_closure"];
    auto osl_mul_closure_color                   = runtime_library_context["osl_mul_closure_color"];
    auto osl_mul_closure_float                   = runtime_library_context["osl_mul_closure_float"];

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

        builder.SetInsertPoint(block);

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

                auto it = function_id.insert({ function_name, 0 }).first;
                function_name = llvm::formatv("call.{0}.{1}", function_name, it->second++);

                auto function_block = llvm::BasicBlock::Create(ll_context, "", function);

                block = llvm::BasicBlock::Create(ll_context, "", function);
                block->setName(llvm::formatv("{0}.{1}", name, block_id++).str());

                auto function_opcode_index = std::exchange(opcode_index, opcode.jump(0));
                auto function_opcode_end = opcode_index;

                stack.push({
                    function_name, 0, function_block, block, nullptr, nullptr, function_opcode_index, function_opcode_end
                });

                builder.CreateBr(function_block);
                builder.SetInsertPoint(block);

                continue;
            }

            if (opname == Ops::u_return) {
                break;
            }

            if (opname == Ops::u_if) {
                auto if_name = llvm::formatv("if.{0}", flow_id++).str();

                llvm::BasicBlock *then_block = nullptr, *else_block = nullptr;

                block = llvm::BasicBlock::Create(ll_context, "", function);
                block->setName(llvm::formatv("{0}.{1}", name, block_id++).str());

                // 'then' clause:
                if (opcode.jump(0) >= opcode_index) {
                    then_block = llvm::BasicBlock::Create(ll_context, "", function);

                    auto begin = std::exchange(opcode_index, opcode.jump(0));
                    auto end = opcode_index;

                    stack.push({
                        if_name + ".then", 0, then_block, block, continue_block, break_block, begin, end
                    });
                }

                // 'else' clause:
                if (opcode.jump(1) > opcode.jump(0)) {
                    else_block = llvm::BasicBlock::Create(ll_context, "", function);

                    auto begin = std::exchange(opcode_index, opcode.jump(1));
                    auto end = opcode_index;

                    stack.push({
                        if_name + ".else", 0, else_block, block, continue_block, break_block, begin, end
                    });
                }

                builder.CreateCondBr(
                    CreateCastToCondition(builder, symbol_scope.getValueOrDereference(opargs[0])),
                    then_block, else_block ? else_block : block);
                builder.SetInsertPoint(block);
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
                auto pred_block = llvm::BasicBlock::Create(ll_context, "", function);

                // cond_block: jump(0) to jump(1)
                auto cond_block = (opcode.jump(1) > opcode.jump(0)) ? llvm::BasicBlock::Create(ll_context, "", function)
                                                                    : pred_block;

                // body_block: jump(1) to jump(2)
                llvm::BasicBlock *body_block = (opcode.jump(2) > opcode.jump(1)) ? llvm::BasicBlock::Create(ll_context, "", function)
                                                                                 : nullptr;

                // ind_block: jump(2) to jump(3)
                llvm::BasicBlock *ind_block = (opcode.jump(3) > opcode.jump(2)) ? llvm::BasicBlock::Create(ll_context, "", function)
                                                                                : nullptr;

                // next_block: jump(2) or jump(3) to opcode_end
                llvm::BasicBlock *exit_block = llvm::BasicBlock::Create(ll_context, "", function);

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

                    llvm::IRBuilder<>::InsertPointGuard guard(builder);
                    builder.SetInsertPoint(pred_block);

                    builder.CreateCondBr(
                        CreateCastToCondition(builder, symbol_scope.getValueOrDereference(opargs[0])),
                        pred_exit, exit_block);
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

                block = llvm::BasicBlock::Create(ll_context, "", function);
                block->setName(llvm::formatv("{0}.{1}", name, block_id++).str());

                builder.CreateBr(continue_block);
                builder.SetInsertPoint(block);

                continue;
            }

            if (opname == Ops::u_break) {
                assert(break_block);

                block = llvm::BasicBlock::Create(ll_context, "", function);

                builder.CreateBr(break_block);
                builder.SetInsertPoint(block);

                continue;
            }

            if (opname == Ops::u_assign) {
                const auto& ltype = opargs[0]->typespec();
                const auto& rtype = opargs[1]->typespec();

                auto lvalue = symbol_scope.getReference(opargs[0]);
                auto rvalue = symbol_scope.getValueOrDereference(opargs[1]);

                if (ltype.is_closure() || ltype.is_matrix() || ltype.is_structure() || ltype.is_string()) {
                    if (ltype.is_closure() && rtype.is_int()) {
                        assert(llvm::isa<llvm::ConstantInt>(rvalue));
                        assert(llvm::cast<llvm::ConstantInt>(rvalue)->isZero());
                        builder.CreateStore(
                            d_context->getLLVMClosureDefaultConstant(),
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

                    builder.CreateStore(rvalue, lvalue);

                    continue;
                }

                rvalue = CreateCast(type_scope, builder,
                                    rtype.simpletype(), rvalue,
                                    ltype.simpletype());

                builder.CreateStore(rvalue, lvalue);

                continue;
            }

            if (opname == Ops::u_add || opname == Ops::u_sub || opname == Ops::u_mul || opname == Ops::u_div) {
                const auto& ltype  = opargs[0]->typespec();
                const auto& rtype0 = opargs[1]->typespec();
                const auto& rtype1 = opargs[2]->typespec();

                auto lvalue  = symbol_scope.getReference(opargs[0]);

                // Closure arithmetic:
                if (ltype.is_closure()) {
                    if (opname == Ops::u_add) {
                        assert(rtype0.is_closure() && rtype1.is_closure());

                        auto closure_value = osl_add_closure_closure(renderer, opargs[1], opargs[2]);
                        builder.CreateStore(closure_value, lvalue);

                        continue;
                    }

                    if (opname == Ops::u_mul) {
                        assert((rtype0.is_closure() && (rtype1.is_triple() || rtype1.is_float())) ||
                               (rtype1.is_closure() && (rtype0.is_triple() || rtype0.is_float())));

                        std::optional<const Symbol *> arg0, arg1;
                        std::optional<bool> color;

                        if (rtype0.is_closure()) {
                            arg0 = opargs[1];
                            arg1 = opargs[2];
                            color = rtype1.is_color();
                        }
                        else if (rtype1.is_closure()) {
                            arg0 = opargs[2];
                            arg1 = opargs[1];
                            color = rtype0.is_color();
                        }

                        assert(arg0 && arg1 && color);

                        auto closure_value = *color
                            ? osl_mul_closure_color(renderer, *arg0, *arg1)
                            : osl_mul_closure_float(renderer, *arg0, *arg1);

                        builder.CreateStore(closure_value, lvalue);

                        continue;
                    }
                }

                assert(!ltype.is_closure());

                // Scalar and vector arithmetic:
                auto rvalue0 = symbol_scope.getValueOrDereference(opargs[1]);
                auto rvalue1 = symbol_scope.getValueOrDereference(opargs[2]);

                rvalue0 = CreateCast(type_scope, builder,
                                     rtype0.simpletype(), rvalue0,
                                     ltype.simpletype());
                rvalue1 = CreateCast(type_scope, builder,
                                     rtype1.simpletype(), rvalue1,
                                     ltype.simpletype());

                const auto [ type, sign, width ] = OSLScalarTypeTraits::get(ltype.simpletype());
                llvm::Value *result = nullptr;

                if (type == OSLScalarTypeTraits::Integer) {
                    if (opname == Ops::u_add) {
                        result = builder.CreateAdd(rvalue0, rvalue1);
                    }
                    else if (opname == Ops::u_sub) {
                        result = builder.CreateSub(rvalue0, rvalue1);
                    }
                    else if (opname == Ops::u_mul) {
                        result = builder.CreateMul(rvalue0, rvalue1);
                    }
                    else if (opname == Ops::u_div) {
                        if (sign) {
                            result = builder.CreateSDiv(rvalue0, rvalue1);
                        }
                        else {
                            result = builder.CreateUDiv(rvalue0, rvalue1);
                        }
                    }
                    else if (opname == Ops::u_mod) {
                        if (sign) {
                            result = builder.CreateSRem(rvalue0, rvalue1);
                        }
                        else {
                            result = builder.CreateURem(rvalue0, rvalue1);
                        }
                    }

                    assert(result);
                }
                else if (type == OSLScalarTypeTraits::Real) {
                    if (opname == Ops::u_add) {
                        result = builder.CreateFAdd(rvalue0, rvalue1);
                    }
                    else if (opname == Ops::u_sub) {
                        result = builder.CreateFSub(rvalue0, rvalue1);
                    }
                    else if (opname == Ops::u_mul) {
                        result = builder.CreateFMul(rvalue0, rvalue1);
                    }
                    else if (opname == Ops::u_div) {
                        result = builder.CreateFDiv(rvalue0, rvalue1);
                    }
                    else if (opname == Ops::u_mod) {
                        result = builder.CreateFRem(rvalue0, rvalue1);
                    }

                    assert(result);
                }

                builder.CreateStore(result, lvalue);

                continue;
            }

            if (opname == Ops::u_neg) {
                const auto& ltype = opargs[0]->typespec();
                const auto& rtype = opargs[1]->typespec();

                auto lvalue = symbol_scope.getReference(opargs[0]);
                auto rvalue = symbol_scope.getValueOrDereference(opargs[1]);

                rvalue = CreateCast(type_scope, builder, rtype.simpletype(), rvalue, ltype.simpletype());

                const auto [ type, sign, width ] = OSLScalarTypeTraits::get(ltype.simpletype());
                llvm::Value *result = nullptr;

                if (type == OSLScalarTypeTraits::Integer) {
                    result = builder.CreateNeg(rvalue);
                }
                else if (type == OSLScalarTypeTraits::Real) {
                    result = builder.CreateFNeg(rvalue);
                }

                builder.CreateStore(result, lvalue);

                continue;
            }

            if (opname == Ops::u_eq || opname == Ops::u_neq || opname == Ops::u_lt || opname == Ops::u_gt || opname == Ops::u_le || opname == Ops::u_ge) {
                const auto& ltype  = opargs[0]->typespec();
                const auto& rtype0 = opargs[1]->typespec();
                const auto& rtype1 = opargs[2]->typespec();

                auto lvalue = symbol_scope.getReference(opargs[0]);
                auto rvalue0 = symbol_scope.getValueOrDereference(opargs[1]);
                auto rvalue1 = symbol_scope.getValueOrDereference(opargs[2]);

                llvm::Value *result = nullptr;

                if (rtype0.is_closure() || rtype1.is_closure()) {
                    assert((rtype0.is_closure() && rtype1.is_int()) ||
                           (rtype1.is_closure() && rtype0.is_int()));
                    assert((rtype0.is_closure() && llvm::isa<llvm::ConstantInt>(rvalue1) && llvm::cast<llvm::ConstantInt>(rvalue1)->isZero()) ||
                           (rtype1.is_closure() && llvm::isa<llvm::ConstantInt>(rvalue0) && llvm::cast<llvm::ConstantInt>(rvalue0)->isZero()));
                    assert(opname == Ops::u_eq || opname == Ops::u_neq);

                    auto closure = rtype0.is_closure() ? rvalue0 : rvalue1;
                    auto closure_data = builder.CreateExtractValue(closure, std::vector<unsigned>{ 0 });

                    if (opname == Ops::u_eq) {
                        result = builder.CreateICmpEQ(
                            closure_data,
                            d_context->getLLVMClosurePointerDefaultConstant());
                    }
                    else if (opname == Ops::u_neq) {
                        result = builder.CreateICmpNE(
                            closure_data,
                            d_context->getLLVMClosurePointerDefaultConstant());
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

                    rvalue0 = CreateCast(type_scope, builder, rtype0.simpletype(), rvalue0, ptype);
                    rvalue1 = CreateCast(type_scope, builder, rtype1.simpletype(), rvalue1, ptype);

                    const auto [ type, sign, width ] = OSLScalarTypeTraits::get(ptype);

                    if (type == OSLScalarTypeTraits::Integer) {
                        if (opname == Ops::u_eq) {
                            result = builder.CreateICmpEQ(rvalue0, rvalue1);
                        }
                        else if (opname == Ops::u_neq) {
                            result = builder.CreateICmpNE(rvalue0, rvalue1);
                        }
                        else if (opname == Ops::u_lt) {
                            if (sign) {
                                result = builder.CreateICmpSLT(rvalue0, rvalue1);
                            }
                            else {
                                result = builder.CreateICmpULT(rvalue0, rvalue1);
                            }
                        }
                        else if (opname == Ops::u_gt) {
                            if (sign) {
                                result = builder.CreateICmpSGT(rvalue0, rvalue1);
                            }
                            else {
                                result = builder.CreateICmpUGT(rvalue0, rvalue1);
                            }
                        }
                        else if (opname == Ops::u_le) {
                            if (sign) {
                                result = builder.CreateICmpSLE(rvalue0, rvalue1);
                            }
                            else {
                                result = builder.CreateICmpULE(rvalue0, rvalue1);
                            }
                        }
                        else if (opname == Ops::u_ge) {
                            if (sign) {
                                result = builder.CreateICmpSGE(rvalue0, rvalue1);
                            }
                            else {
                                result = builder.CreateICmpUGE(rvalue0, rvalue1);
                            }
                        }
                    }
                    else if (type == OSLScalarTypeTraits::Real) {
                        if (opname == Ops::u_eq) {
                            result = builder.CreateFCmpUEQ(rvalue0, rvalue1);
                        }
                        else if (opname == Ops::u_neq) {
                            result = builder.CreateFCmpUNE(rvalue0, rvalue1);
                        }
                        else if (opname == Ops::u_lt) {
                            result = builder.CreateFCmpULT(rvalue0, rvalue1);
                        }
                        else if (opname == Ops::u_gt) {
                            result = builder.CreateFCmpUGT(rvalue0, rvalue1);
                        }
                        else if (opname == Ops::u_le) {
                            result = builder.CreateFCmpULE(rvalue0, rvalue1);
                        }
                        else if (opname == Ops::u_ge) {
                            result = builder.CreateFCmpUGE(rvalue0, rvalue1);
                        }
                    }

                    if (ptype.aggregate != OSL::TypeDesc::SCALAR) {
                        result = builder.CreateAndReduce(result);
                    }
                }

                assert(result);

                result = builder.CreateZExt(
                    result,
                    d_context->getLLVMType(
                        OSL::TypeDesc((OSL::TypeDesc::BASETYPE)ltype.simpletype().basetype),
                        false));

                builder.CreateStore(result, lvalue);

                continue;
            }

            if (opname == Ops::u_bitand || opname == Ops::u_bitor || opname == Ops::u_xor || opname == Ops::u_shl || opname == Ops::u_shr) {
                const auto& ltype  = opargs[0]->typespec();
                const auto& rtype0 = opargs[1]->typespec();
                const auto& rtype1 = opargs[2]->typespec();

                auto lvalue  = symbol_scope.getReference(opargs[0]);
                auto rvalue0 = symbol_scope.getValueOrDereference(opargs[1]);
                auto rvalue1 = symbol_scope.getValueOrDereference(opargs[2]);

                rvalue0 = CreateCast(type_scope, builder,
                                     rtype0.simpletype(), rvalue0,
                                     ltype.simpletype());
                rvalue1 = CreateCast(type_scope, builder,
                                     rtype1.simpletype(), rvalue1,
                                     ltype.simpletype());

                llvm::Value *result = nullptr;

                if (opname == Ops::u_bitand) {
                    result = builder.CreateAnd(rvalue0, rvalue1);
                }
                else if (opname == Ops::u_bitor) {
                    result = builder.CreateOr(rvalue0, rvalue1);
                }
                else if (opname == Ops::u_xor) {
                    result = builder.CreateXor(rvalue0, rvalue1);
                }
                else if (opname == Ops::u_shl) {
                    result = builder.CreateShl(rvalue0, rvalue1);
                }
                else if (opname == Ops::u_shr) {
                    result = builder.CreateAShr(rvalue0, rvalue1);
                }

                builder.CreateStore(result, lvalue);

                continue;
            }

            if (opname == Ops::u_compl) {
                const auto& ltype = opargs[0]->typespec();
                const auto& rtype = opargs[1]->typespec();

                auto lvalue = symbol_scope.getReference(opargs[0]);
                auto rvalue = symbol_scope.getValueOrDereference(opargs[1]);

                rvalue = CreateCast(type_scope, builder, rtype.simpletype(), rvalue, ltype.simpletype());

                builder.CreateStore(
                    builder.CreateNeg(rvalue),
                    lvalue);

                continue;
            }

            if (opname == Ops::u_closure) {
                auto it_arg = opargs.begin();
                auto lvalue  = symbol_scope.getReference((*it_arg++));

                auto weight = (!(*it_arg)->typespec().is_string()) ? symbol_scope.getValueOrDereference((*it_arg++)) : nullptr;

                auto closure_name = (*it_arg++)->get_string();
                auto closure = d_context->getClosure(llvm::StringRef(closure_name.data(), closure_name.length()));

                // TODO: better error signalling:
                assert(closure);

                // Create the closure component:
                auto params_type = closure->params_type();
                auto params_type_size = d_module->getDataLayout().getStructLayout(params_type)->getSizeInBytes();

                auto closure_value = weight
                    ? osl_allocate_weighted_closure_component(renderer, closure->id(), params_type_size, weight)
                    : osl_allocate_closure_component(renderer, closure->id(), params_type_size);

                // Load parameters:
                auto params =
                    builder.CreateBitCast(
                        builder.CreateExtractValue(closure_value, std::vector<unsigned>{ 0 }),
                        llvm::PointerType::get(params_type, d_context->bxdf_address_space()));

                struct Frame {
                    OSL::TypeDesc type;
                    llvm::Value *lhs_value = nullptr, *rhs_value = nullptr;
                };

                std::stack<Frame> stack;

                auto     rhs_it = opargs.rbegin(), rhs_it_end = std::vector<const Symbol *>::reverse_iterator(it_arg);

                // TODO: better error signalling:
                assert(std::distance(rhs_it, rhs_it_end) == closure->param_count());

                auto     lhs_type_index  = closure->param_rbegin();
                unsigned lhs_value_index = closure->param_count() - 1;

                for (; rhs_it != rhs_it_end; ++rhs_it, ++lhs_type_index, --lhs_value_index) {
                    auto arg = *rhs_it;

                    const auto& ltype = *lhs_type_index;
                    const auto& rtype = arg->typespec().simpletype();

                    auto lvalue = builder.CreateStructGEP(nullptr, params, lhs_value_index);
                    auto rvalue = CreateCast(type_scope, builder, rtype, symbol_scope.getValueOrDereference(arg), ltype);

                    stack.push({
                        ltype, lvalue, rvalue
                    });
                }

                while (!stack.empty()) {
                    auto [ t, lvalue, rvalue ] = stack.top();
                    stack.pop();

                    OSL::TypeDesc::BASETYPE  basetype  = (OSL::TypeDesc::BASETYPE)t.basetype;
                    OSL::TypeDesc::AGGREGATE aggregate = (OSL::TypeDesc::AGGREGATE)t.aggregate;
                    auto arraylen  = t.arraylen;

                    if (arraylen > 0) {
                        // TODO
                        continue;
                    }

                    if (aggregate != OSL::TypeDesc::SCALAR) {
                        unsigned n = 1;
                        OSL::TypeDesc::AGGREGATE column_aggregate = OSL::TypeDesc::SCALAR;

                        switch (aggregate) {
                            case OSL::TypeDesc::VEC2    : n = 2; break;
                            case OSL::TypeDesc::VEC3    : n = 3; break;
                            case OSL::TypeDesc::VEC4    : n = 4; break;
                            case OSL::TypeDesc::MATRIX33: n = 3; column_aggregate = OSL::TypeDesc::VEC3; break;
                            case OSL::TypeDesc::MATRIX44: n = 4; column_aggregate = OSL::TypeDesc::VEC4; break;
                            default: break;
                        }

                        switch (aggregate) {
                            case OSL::TypeDesc::VEC2:
                            case OSL::TypeDesc::VEC3:
                            case OSL::TypeDesc::VEC4:
                                for (unsigned i = 0; i < n; ++i) {
                                    builder.CreateStore(
                                        builder.CreateExtractElement(rvalue, i),
                                        builder.CreateConstInBoundsGEP2_32(nullptr, lvalue, 0, i));
                                }

                                continue;
                            case OSL::TypeDesc::MATRIX33:
                            case OSL::TypeDesc::MATRIX44:
                                // TODO
                                continue;
                            default:
                                continue;
                        }
                    }

                    builder.CreateStore(rvalue, lvalue);
                }

                builder.CreateStore(closure_value, lvalue);

                continue;
            }
        }

        builder.CreateBr(merge_block);

        builder.ClearInsertionPoint();
    }

    SortBasicBlocksTopologically(function);

    // Parameter metadata:
    std::vector<llvm::Metadata *> parameter_mds;
    std::transform(
        d_parameters, d_parameters + d_parameter_count,
        std::back_inserter(parameter_mds),
            [](auto& parameter) -> llvm::Metadata * {
                return parameter.d_md.get();
        });

    d_parameters_md.reset(
        llvm::MDTuple::get(ll_context, parameter_mds));

    function = processBXDFs(function);

    d_main_function_md.reset(
        llvm::ValueAsMetadata::get(function));

    // All metadata:
    d_md.reset(
        llvm::MDTuple::get(ll_context, {
            d_main_function_md.get(),
            d_parameters_md.get(),
            d_bxdf_md.get()
        }));

    auto shaders_md = d_module->getOrInsertNamedMetadata("llosl.shaders");
    shaders_md->addOperand(d_md.get());

    // Other optimizations:
    auto mpm = std::make_unique<llvm::legacy::PassManager>();
    mpm->add(llvm::createReassociatePass());
    mpm->add(llvm::createSCCPPass());
    mpm->add(llvm::createAggressiveDCEPass());
    mpm->add(llvm::createCFGSimplificationPass());
    mpm->add(llvm::createPromoteMemoryToRegisterPass());
    mpm->run(*d_module);
}

Shader::~Shader() {
}

llvm::Function *
Shader::processBXDFs(llvm::Function *function) {
    auto& ll_context = d_context->getLLContext();

    auto function_name = function->getName().str();

    // Instrument the function with path information, and collect information
    // about the BXDFs:
    auto closure_ir = new ClosureIRPass();
    auto path_info = new PathInfoPass();
    auto instrumentation = new InstrumentationPass();
    auto bxdf = new BXDFPass();

    llvm::legacy::FunctionPassManager fpm(d_module.get());

    fpm.add(closure_ir);
    fpm.add(path_info);
    fpm.add(instrumentation);
    fpm.add(bxdf);
    fpm.run(*function);

    // `function` was rewritten:
    function = d_module->getFunction(function_name);
    assert(function);

    // BXDFs:
    d_bxdf_info = bxdf->getBXDFInfo();

    unsigned path_count = d_bxdf_info->getPathCount();

    d_bxdfs.reserve(path_count);
    auto it_bxdf = std::back_inserter(d_bxdfs);

    auto int16_type = llvm::Type::getInt16Ty(ll_context);
    auto path_id_to_index_type = llvm::ArrayType::get(int16_type, path_count);

    std::vector<llvm::Constant *> path_id_to_index;
    path_id_to_index.reserve(path_count);
    auto it_path_id_to_index = std::back_inserter(path_id_to_index);

    // Metadata:
    std::vector<llvm::Metadata *> bxdf_mds;
    auto it = std::back_inserter(bxdf_mds);

    // First node is the number of paths:
    *it++ = llvm::ConstantAsMetadata::get(
        llvm::ConstantInt::get(ll_context, llvm::APInt(32, path_count)));

    // Second node is the max heap size:
    *it++ = llvm::ConstantAsMetadata::get(
        llvm::ConstantInt::get(ll_context, llvm::APInt(32, d_bxdf_info->getMaxHeapSize())));

    // Remaining nodes are repeating tuples of (path_id, heap_size, encoding):
    for (unsigned path_id = 0; path_id < path_count; ++path_id) {
        const auto& bxdf_info = d_bxdf_info->getBXDFForPath(path_id);
        auto encoding = BXDFAST::encode(bxdf_info.ast);

        auto [ bxdf, index, inserted ] = d_context->getOrInsertBXDF(encoding, bxdf_info.ast, bxdf_info.heap_size);

        *it_bxdf = bxdf;

        *it_path_id_to_index = llvm::ConstantInt::get(int16_type, index);

        // Metadata:
        *it++ = llvm::MDTuple::get(ll_context, {
            llvm::ConstantAsMetadata::get(
                llvm::ConstantInt::get(ll_context, llvm::APInt(32, path_id))),
            llvm::ConstantAsMetadata::get(
                llvm::ConstantInt::get(ll_context, llvm::APInt(32, bxdf_info.heap_size))),
            llvm::MDString::get(ll_context,
                llvm::StringRef(reinterpret_cast<const char *>(encoding.data()),
                                encoding.size()))
        });
    }

    auto path_id_to_index_value = new llvm::GlobalVariable(
        *d_module, path_id_to_index_type, true,
        llvm::GlobalVariable::InternalLinkage,
        llvm::ConstantArray::get(path_id_to_index_type, path_id_to_index),
        "LLOSLPathIdToIndex", nullptr,
        llvm::GlobalVariable::NotThreadLocal, 2);

    // Create a function that calls the main function and maps the
    // resulting path id to an index into the uber BXDF:
    auto mapping_function = llvm::Function::Create(
        function->getFunctionType(),
        llvm::GlobalValue::ExternalLinkage, function->getName().str() + "_mapped", d_module.get());

  { auto entry_block = llvm::BasicBlock::Create(ll_context, "entry", mapping_function);

    llvm::IRBuilder<> builder(ll_context);
    builder.SetInsertPoint(entry_block);

    std::vector<llvm::Value *> args;
    args.reserve(mapping_function->arg_size());
    std::transform(
        mapping_function->arg_begin(), mapping_function->arg_end(),
        std::back_inserter(args),
        [](auto& arg) -> llvm::Value *{
            return &arg;
        });

    auto path_id = builder.CreateCall(function, args);

    auto index = builder.CreateLoad(
        builder.CreateGEP(path_id_to_index_value, std::vector<llvm::Value *>{
            path_id }));

    builder.CreateRet(path_id); }

    d_bxdf_md.reset(
        llvm::MDTuple::get(ll_context, bxdf_mds));

    return function;
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

Shader::Parameter::Parameter(bool is_output, llvm::Type *llvm_type, unsigned index, llvm::ArrayRef<OSL::ustring> name, bool is_closure, const OSL::TypeDesc& osl_type, Shader *parent)
: d_parent(parent)
, d_is_output(is_output)
, d_type(llvm_type)
, d_is_closure(is_closure) {
    auto& ll_context = d_parent->module()->getContext();

    std::vector<llvm::Metadata *> mds;
    mds.reserve(8 + (d_is_output? 1 : 0) + (d_is_closure? 1 : 0));

    auto it = std::back_inserter(mds);

    *it++ = llvm::ConstantAsMetadata::get(
        llvm::ConstantInt::get(ll_context, llvm::APInt(32, index, true)));
    *it++ = llvm::MDString::get(ll_context, "llosl.parameter_name");

    std::vector<llvm::Metadata *> name_mds;
    name_mds.reserve(name.size());

    std::transform(
        name.begin(), name.end(),
        std::back_inserter(name_mds),
        [&ll_context](auto name) -> auto {
            return llvm::MDString::get(ll_context, name.c_str());
        });

    *it++ = llvm::MDTuple::get(ll_context, name_mds);
    *it++ = llvm::MDString::get(ll_context, "llosl.type");
    *it++ = llvm::ConstantAsMetadata::get(
        llvm::ConstantInt::get(ll_context, llvm::APInt(8, osl_type.basetype, true)));
    *it++ = llvm::ConstantAsMetadata::get(
        llvm::ConstantInt::get(ll_context, llvm::APInt(8, osl_type.aggregate, true)));
    *it++ = llvm::ConstantAsMetadata::get(
        llvm::ConstantInt::get(ll_context, llvm::APInt(8, osl_type.vecsemantics, true)));
    *it++ = llvm::ConstantAsMetadata::get(
        llvm::ConstantInt::get(ll_context, llvm::APInt(32, osl_type.arraylen, true)));
    if (d_is_closure) *it++ = llvm::MDString::get(ll_context, "llosl.closure");
    if (d_is_output) *it++ = llvm::MDString::get(ll_context, "llosl.output_parameter");

    d_md.reset(
        llvm::MDTuple::get(ll_context, mds));
}

unsigned
Shader::Parameter::index() const {
    auto md = llvm::mdconst::dyn_extract<llvm::ConstantInt>(d_md->getOperand(0).get());
    assert(md);

	return md->getZExtValue();
}

llvm::StringRef
Shader::Parameter::name(unsigned index) const {
    auto name = llvm::cast<llvm::MDNode>(d_md->getOperand(2).get());
    auto name_part = llvm::cast<llvm::MDString>(d_md->getOperand(index).get());

    return name_part->getString();
}

llvm::Type *
Shader::Parameter::llvm_type() const {
    return d_type;
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
