#include <llosl/IR/ClosureIRPass.h>
#include <llosl/IR/ClosureFunction.h>
#include <llosl/IR/Instruction.h>

#include <llvm/ADT/DenseMapInfo.h>
#include <llvm/ADT/DenseSet.h>
#include <llvm/ADT/Optional.h>
#include <llvm/ADT/SCCIterator.h>
#include <llvm/Analysis/BasicAliasAnalysis.h>
#include <llvm/Analysis/MemoryLocation.h>
#include <llvm/IR/CFG.h>
#include <llvm/IR/Constants.h>
#include <llvm/IR/Instructions.h>

#include <algorithm>
#include <iostream>
#include <list>
#include <stack>

namespace llvm {

void initializeClosureIRPassPass(PassRegistry&);

} // End namespace llvm

namespace llosl {

class ClosureIRPass::Context {
public:

    Context(llvm::Function&, llvm::AAResults&);

    llvm::Function *osl_init_closure_storage                = nullptr,
                   *osl_add_closure_closure                 = nullptr,
                   *osl_mul_closure_color                   = nullptr,
                   *osl_mul_closure_float                   = nullptr,
                   *osl_allocate_closure_component          = nullptr,
                   *osl_allocate_weighted_closure_component = nullptr;

    ClosureFunction *getFunction() const { return d_closure_function.get(); }

    llvm::GetElementPtrInst *Ci() const { return d_Ci.get(); }

    using SortedBlocks = std::list<std::vector<llvm::BasicBlock *> >;
    const SortedBlocks& getBlocksInTopologicalOrder() const { return d_sorted_blocks; };
    SortedBlocks::const_iterator blocks_begin() const { return d_sorted_blocks.begin(); }
    SortedBlocks::const_iterator blocks_end() const { return d_sorted_blocks.end(); }

    //
    unsigned insertClosureStorage(llvm::MemoryLocation);
    llvm::Optional<unsigned> findClosureStorage(llvm::MemoryLocation) const;

    //
    void beginFunction();
    std::unique_ptr<ClosureFunction> endFunction();

    ClosureFunction *function()             { return d_closure_function.get(); };
    const ClosureFunction *function() const { return d_closure_function.get(); };

    //
    void beginBlock(const llvm::BasicBlock&);
    void endBlock();

    Block *block()             { return d_block; };
    const Block *block() const { return d_block; };

    //
    Alloca                    *createAlloca(const llvm::CallInst&);
    AllocateComponent         *createAllocateComponent(const llvm::CallInst&);
    AllocateWeightedComponent *createAllocateWeightedComponent(const llvm::CallInst&);
    AddClosureClosure         *createAddClosureClosure(const llvm::CallInst&);
    MulClosureColor           *createMulClosureColor(const llvm::CallInst&);
    MulClosureFloat           *createMulClosureFloat(const llvm::CallInst&);
    Load                      *createLoad(const llvm::LoadInst&);
    Store                     *createStore(const llvm::StoreInst&);
    Cast                      *createCast(const llvm::CastInst&);
    PHI                       *createPHI(const llvm::PHINode&);

    //
    void insertValue(Value *);
    void insertValue(const llvm::Value *, Value *);
    llvm::Optional<Value *> findValue(const llvm::Value *);

private:

    enum class State {
        ClosureStorage,
        Function,
        Block,
        Final
    };

    Block *getBlock();

    State d_state = State::ClosureStorage;

    llvm::LLVMContext& d_ll_context;
    llvm::Module& d_module;
    llvm::Function& d_function;
    llvm::AAResults& d_aa;

    SortedBlocks d_sorted_blocks;

    // Memory locations that are known to refer to closures:
    std::unique_ptr<llvm::GetElementPtrInst> d_Ci;

    mutable llvm::DenseMap<llvm::MemoryLocation, unsigned> d_closure_locations;
    unsigned d_closure_storage_count = 0;

    // The currently evolving function:
    std::unique_ptr<ClosureFunction> d_closure_function;

    // The block being processed:
    const llvm::BasicBlock *d_ll_block = nullptr;
    Block *d_block = nullptr;

    // Values that are known to represent closures:
    llvm::DenseMap<const llvm::Value *, Value *> d_value_map;
};

ClosureIRPass::Context::Context(llvm::Function& function, llvm::AAResults& aa)
: d_ll_context(function.getParent()->getContext())
, d_module(*function.getParent())
, d_function(function)
, d_aa(aa) {
    osl_init_closure_storage                = d_module.getFunction("osl_init_closure_storage");
    osl_add_closure_closure                 = d_module.getFunction("osl_add_closure_closure");
    osl_mul_closure_color                   = d_module.getFunction("osl_mul_closure_color");
    osl_mul_closure_float                   = d_module.getFunction("osl_mul_closure_float");
    osl_allocate_closure_component          = d_module.getFunction("osl_allocate_closure_component");
    osl_allocate_weighted_closure_component = d_module.getFunction("osl_allocate_weighted_closure_component");

    //
    std::copy(
        llvm::scc_iterator<llvm::Function *>::begin(&d_function), llvm::scc_iterator<llvm::Function *>::end(&d_function),
        std::back_inserter(d_sorted_blocks));

    std::reverse(d_sorted_blocks.begin(), d_sorted_blocks.end());

    //
    auto globals = d_function.arg_begin();

    d_Ci.reset(
        llvm::GetElementPtrInst::Create(
            nullptr,
            globals, { llvm::ConstantInt::get(llvm::Type::getInt64Ty(d_ll_context),  0, true),
	                   llvm::ConstantInt::get(llvm::Type::getInt32Ty(d_ll_context), 20, true)  }));

    insertClosureStorage(llvm::MemoryLocation(d_Ci.get(), 1));
}

unsigned
ClosureIRPass::Context::insertClosureStorage(llvm::MemoryLocation location) {
    assert(d_state == State::ClosureStorage);

    auto tmp = d_closure_locations.insert({ location, d_closure_storage_count++});
    assert(tmp.second);

    return tmp.first->second;
}

llvm::Optional<unsigned>
ClosureIRPass::Context::findClosureStorage(llvm::MemoryLocation location) const {
    // First try to find `location` in the map:
    auto it = d_closure_locations.find(location);

    // Even if `location` is not in the map, it might alias something that is:
    if (it == d_closure_locations.end()) {
        auto jt = std::find_if(
            d_closure_locations.begin(), d_closure_locations.end(),
            [this, location](auto tmp) -> bool {
                return d_aa.alias(location, tmp.first) == llvm::MustAlias;
            }
        );
        if (jt != d_closure_locations.end()) {
            it = d_closure_locations.insert({ location, jt->second }).first;
        }
    }

    if (it != d_closure_locations.end()) {
        return { it->second };
    }
    else {
        return llvm::Optional<unsigned>();
    }
}

void
ClosureIRPass::Context::beginFunction() {
    assert(d_state == State::ClosureStorage);

    d_closure_function = std::make_unique<ClosureFunction>(d_function, d_closure_storage_count);

    d_state = State::Function;
}

std::unique_ptr<ClosureFunction>
ClosureIRPass::Context::endFunction() {
    assert(d_state == State::Function);

    d_state = State::Final;

    return std::move(d_closure_function);
}

void
ClosureIRPass::Context::beginBlock(const llvm::BasicBlock& ll_block) {
    assert(d_state == State::Function);

    d_ll_block = &ll_block;

    d_state = State::Block;
}

void
ClosureIRPass::Context::endBlock() {
    assert(d_state == State::Block);

    d_ll_block = nullptr;
    d_block = nullptr;

    d_state = State::Function;
}

Block *
ClosureIRPass::Context::getBlock() {
    assert(d_state == State::Block);

    if (!d_block) {
        d_block = new Block(*d_ll_block, d_closure_function.get());
        insertValue(d_block);
    }

    return d_block;
}

Alloca *
ClosureIRPass::Context::createAlloca(const llvm::CallInst& call_instruction) {
    auto block = getBlock();

    auto storage = call_instruction.getArgOperand(1);
    auto cast_instruction = llvm::dyn_cast<llvm::CastInst>(storage);
    assert(cast_instruction);

    storage = cast_instruction->getOperand(0);
    auto alloca_instruction = llvm::dyn_cast<llvm::AllocaInst>(storage);
    assert(alloca_instruction);

    llvm::MemoryLocation location(alloca_instruction, 1);
    auto tmp = findClosureStorage(location);
    assert(tmp);

    auto instruction = new Alloca(*alloca_instruction, *tmp, block);
    insertValue(instruction);
    insertValue(&call_instruction, instruction);
    return instruction;
}

AllocateComponent *
ClosureIRPass::Context::createAllocateComponent(const llvm::CallInst& call_instruction) {
    auto block = getBlock();
    auto instruction = new AllocateComponent(call_instruction, block);
    insertValue(instruction);
    return instruction;
}

AllocateWeightedComponent *
ClosureIRPass::Context::createAllocateWeightedComponent(const llvm::CallInst& call_instruction) {
    auto block = getBlock();
    auto instruction = new AllocateWeightedComponent(call_instruction, block);
    insertValue(instruction);
    return instruction;
}

AddClosureClosure *
ClosureIRPass::Context::createAddClosureClosure(const llvm::CallInst& call_instruction) {
    auto block = getBlock();

    auto lhs = findValue(call_instruction.getArgOperand(1));
    assert(lhs);

    auto rhs = findValue(call_instruction.getArgOperand(2));
    assert(rhs);

    auto instruction = new AddClosureClosure(call_instruction, **lhs, **rhs, block);
    insertValue(instruction);
    return instruction;
}

MulClosureColor *
ClosureIRPass::Context::createMulClosureColor(const llvm::CallInst& call_instruction) {
    auto block = getBlock();

    auto lhs = findValue(call_instruction.getArgOperand(1));
    assert(lhs);

    auto instruction = new MulClosureColor(call_instruction, **lhs, block);
    insertValue(instruction);
    return instruction;
}

MulClosureFloat *
ClosureIRPass::Context::createMulClosureFloat(const llvm::CallInst& call_instruction) {
    auto block = getBlock();

    auto lhs = findValue(call_instruction.getArgOperand(1));
    assert(lhs);

    auto instruction = new MulClosureFloat(call_instruction, **lhs, block);
    insertValue(instruction);
    return instruction;
}

Load *
ClosureIRPass::Context::createLoad(const llvm::LoadInst& load_instruction) {
    auto location = llvm::MemoryLocation::get(&load_instruction);
    auto tmp = findClosureStorage(location);
    if (!tmp) {
        return nullptr;
    }

    auto block = getBlock();
    auto instruction = new Load(load_instruction, *tmp, block);
    insertValue(instruction);
    return instruction;
}

Store *
ClosureIRPass::Context::createStore(const llvm::StoreInst& store_instruction) {
    auto location = llvm::MemoryLocation::get(&store_instruction);
    auto tmp = findClosureStorage(location);
    if (!tmp) {
        return nullptr;
    }

    auto rhs = findValue(store_instruction.getValueOperand());
    assert(rhs);

    auto block = getBlock();
    auto instruction = new Store(store_instruction, **rhs, *tmp, block);
    insertValue(instruction);
    return instruction;
}

Cast *
ClosureIRPass::Context::createCast(const llvm::CastInst& cast_instruction) {
    auto rhs = findValue(cast_instruction.getOperand(0));
    if (!rhs) {
        return nullptr;
    }

    auto block = getBlock();
    auto instruction = new Cast(cast_instruction, **rhs, block);
    insertValue(instruction);
    return instruction;
}

PHI *
ClosureIRPass::Context::createPHI(const llvm::PHINode& phi_node) {
    std::vector<Value *> operands;
    std::for_each(
        phi_node.op_begin(), phi_node.op_end(),
        [this, &operands](const auto& ll_operand) -> void {
            auto operand = findValue(ll_operand.get());
            if (!operand) {
                return;
            }

            operands.push_back(*operand);
        });

    if (operands.empty()) {
        return nullptr;
    }

    auto block = getBlock();
    auto instruction = new PHI(phi_node, operands, block);
    insertValue(instruction);
    return instruction;
}

void
ClosureIRPass::Context::insertValue(Value *value) {
    insertValue(value->getLLValue(), value);
}

void
ClosureIRPass::Context::insertValue(const llvm::Value *ll_value, Value *value) {
    d_value_map.insert({ ll_value, value });
}

llvm::Optional<Value *>
ClosureIRPass::Context::findValue(const llvm::Value *ll_value) {
    auto it = d_value_map.find(ll_value);
    if (it != d_value_map.end()) {
        return llvm::Optional<Value *>(it->second);
    }
    else {
        return llvm::Optional<Value *>();
    }
}

ClosureIRPass::ClosureIRPass() : FunctionPass(ID) {
    llvm::initializeClosureIRPassPass(*llvm::PassRegistry::getPassRegistry());
}

char ClosureIRPass::ID = 0;

llvm::FunctionPass *createClosureIRPass() {
    return new ClosureIRPass();
}

bool ClosureIRPass::runOnFunction(llvm::Function &F) {
    std::cerr << __PRETTY_FUNCTION__ << std::endl;

    Context context(F, getAnalysis<llvm::AAResultsWrapperPass>().getAAResults());

    F.dump();

    // Determine closure storage:
    std::for_each(
        context.blocks_begin(), context.blocks_end(),
        [this, &context](const auto& scc) -> void {
            std::for_each(
                scc.begin(), scc.end(),
                [this, &context](const auto block) -> void {
                    std::for_each(
                        block->begin(), block->end(),
                        [this, &context](const auto& ll_instruction) -> void {
                            auto call_instruction = llvm::dyn_cast<llvm::CallInst>(&ll_instruction);
                            if (!call_instruction) {
                                return;
                            }

                            auto called_function = call_instruction->getCalledValue();

                            if (called_function != context.osl_init_closure_storage) {
                                return;
                            }

                            auto storage = call_instruction->getArgOperand(1);
                            auto cast_instruction = llvm::dyn_cast<llvm::CastInst>(storage);
                            assert(cast_instruction);

                            storage = cast_instruction->getOperand(0);
                            auto alloca_instruction = llvm::dyn_cast<llvm::AllocaInst>(storage);
                            assert(alloca_instruction);

                            context.insertClosureStorage(llvm::MemoryLocation(alloca_instruction, 1));
                        });
                });
        });

    // Create instructions:
    context.beginFunction();

    std::for_each(
        context.blocks_begin(), context.blocks_end(),
        [this, &context](const auto& scc) -> void {
            std::for_each(
                scc.begin(), scc.end(),
                [this, &context](const auto block) -> void {
                    context.beginBlock(*block);

                    std::for_each(
                        block->begin(), block->end(),
                        [this, &context](const auto& ll_instruction) -> void {
                            switch (ll_instruction.getOpcode()) {
                            case llvm::Instruction::Call: {
                                auto call_instruction = llvm::cast<llvm::CallInst>(&ll_instruction);
                                auto called_function = call_instruction->getCalledValue();

                                if (called_function == context.osl_init_closure_storage) {
                                    context.createAlloca(*call_instruction);
                                }
                                else if (called_function == context.osl_allocate_closure_component) {
                                    context.createAllocateComponent(*call_instruction);
                                }
                                else if (called_function == context.osl_allocate_weighted_closure_component) {
                                    context.createAllocateWeightedComponent(*call_instruction);
                                }
                                else if (called_function == context.osl_add_closure_closure) {
                                    context.createAddClosureClosure(*call_instruction);
                                }
                                else if (called_function == context.osl_mul_closure_color) {
                                    context.createMulClosureColor(*call_instruction);
                                }
                                else if (called_function == context.osl_mul_closure_float) {
                                    context.createMulClosureFloat(*call_instruction);
                                }
                            } break;
                            case llvm::Instruction::Load: {
                                auto load_instruction = llvm::cast<llvm::LoadInst>(&ll_instruction);
                                context.createLoad(*load_instruction);
                            } break;
                            case llvm::Instruction::Store: {
                                auto store_instruction = llvm::cast<llvm::StoreInst>(&ll_instruction);
                                context.createStore(*store_instruction);
                            } break;
                            case llvm::Instruction::PtrToInt:
			                case llvm::Instruction::IntToPtr:
			                case llvm::Instruction::BitCast: {
                                auto cast_instruction = llvm::dyn_cast<llvm::CastInst>(&ll_instruction);
                                context.createCast(*cast_instruction);
                            } break;
                            case llvm::Instruction::PHI: {
                                auto phi_node = llvm::cast<llvm::PHINode>(&ll_instruction);
                                context.createPHI(*phi_node);
                            } break;
                            default:
                                break;
                            };
                        });

                    context.endBlock();
                });
        });

    // Construct the CFG:
    {
        llvm::DenseMap<const llvm::BasicBlock *, llvm::DenseSet<const llvm::BasicBlock *> > cfg, cfg_back;

        struct Frame {
            const llvm::BasicBlock *block = nullptr;
            bool back = false;
        };

        std::stack<Frame> stack;

        enum class Color {
            White, Grey, Black
        };

        llvm::DenseMap<const llvm::BasicBlock *, Color> color;

        std::for_each(
            F.begin(), F.end(),
            [&stack, &color](const auto& block) -> void {
                color[&block] = Color::White;

                if (llvm::succ_begin(&block) == llvm::succ_end(&block)) {
                    stack.push({ &block, false });
                }
            });

        // Duplicate the LLVM IR CFG:
        while (!stack.empty()) {
            auto block = stack.top().block;
            auto back = stack.top().back;

            stack.pop();

            if (!back) {
                color[block] = Color::Grey;

                std::for_each(
                    llvm::pred_begin(block), llvm::pred_end(block),
                    [&stack, &color, &cfg, &cfg_back, block](auto pred) -> void {
                        cfg[pred].insert(block);
                        cfg_back[block].insert(pred);

                        if (color[pred] == Color::White) {
                            stack.push({ pred, false });
                        }
                    });
            }
            else {
                color[block] = Color::Black;
            }
        }

        // Remove blocks that have nothing to do with closures:
        std::for_each(
            context.blocks_begin(), context.blocks_end(),
            [this, &context, &cfg, &cfg_back](const auto& scc) -> void {
                std::for_each(
                    scc.begin(), scc.end(),
                    [this, &context, &cfg, &cfg_back](const auto ll_block) -> void {
                        auto block = context.findValue(ll_block);
                        if (block) {
                            return;
                        }

                        // Link each of ll_block's successors to its predecessors, and
                        // remove ll_block itself from cfg_back:
                        const auto& succs = cfg[ll_block];
                        std::for_each(
                            succs.begin(), succs.end(),
                            [&cfg_back, ll_block](auto succ) -> void {
                                const auto& preds = cfg_back[ll_block];
                                std::for_each(
                                    preds.begin(), preds.end(),
                                    [&cfg_back, succ](auto pred) -> void {
                                        cfg_back[succ].insert(pred);
                                    });

                                cfg_back[succ].erase(ll_block);
                            });

                        //
                        const auto& preds = cfg_back[ll_block];
                        std::for_each(
                            preds.begin(), preds.end(),
                            [&cfg, ll_block](auto pred) -> void {
                                const auto& succs = cfg[ll_block];
                                std::for_each(
                                    succs.begin(), succs.end(),
                                    [&cfg, pred](auto succ) -> void {
                                        cfg[pred].insert(succ);
                                    });

                                cfg[pred].erase(ll_block);
                            });

                        cfg.erase(ll_block);
                        cfg_back.erase(ll_block);
                    });
            });

        std::for_each(
            cfg.begin(), cfg.end(),
            [&context](const auto &tmp) -> void {
                auto value = context.findValue(tmp.first);
                assert(value);

                auto block = llvm::cast<Block>(*value);
                assert(block);

                std::for_each(
                    tmp.second.begin(), tmp.second.end(),
                    [&context, block](auto ll_succ) -> void {
                        auto value = context.findValue(ll_succ);
                        assert(value);

                        auto succ = llvm::cast<Block>(*value);
                        assert(succ);

                        block->insertSuccessor(succ);
                    });
            });

        std::for_each(
            context.function()->blocks_begin(), context.function()->blocks_end(),
            [&cfg](const auto& block) -> void {
                std::cerr << block.getLLValue()->getName().str() << std::endl;

                std::for_each(
                    block.succs_begin(), block.succs_end(),
                    [](auto succ) -> void {
                        std::cerr << "\t" << succ->getLLValue()->getName().str() << std::endl;
                    });
            });
    }

    auto function = context.endFunction();

    std::cerr << function->getClosureStorageCount() << " closure slots" << std::endl;

#if 0
    std::for_each(
        function->blocks_begin(), function->blocks_end(),
        [](const auto& block) {
            std::cerr << "block" << std::endl;

            std::for_each(
                block.insts_begin(), block.insts_end(),
                [](const auto& inst) {
                    inst.dump();
                }
            );
        });
#endif

    return true;
}

void ClosureIRPass::getAnalysisUsage(llvm::AnalysisUsage &AU) const {
    AU.setPreservesAll();
    AU.addRequired<llvm::AAResultsWrapperPass>();
}

} // End namespace llosl

using llosl::ClosureIRPass;
using namespace llvm;

INITIALIZE_PASS_BEGIN(ClosureIRPass, "llosl-closure-ir",
                      "ClosureIR", false, false)
INITIALIZE_PASS_DEPENDENCY(AAResultsWrapperPass)
INITIALIZE_PASS_END(ClosureIRPass, "llosl-closure-info",
                    "ClosureIR", false, false)
