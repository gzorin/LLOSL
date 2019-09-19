#include <llosl/IR/ClosureIRPass.h>
#include <llosl/IR/ClosureFunction.h>
#include <llosl/IR/Instruction.h>

#include <llvm/ADT/DenseMapInfo.h>
#include <llvm/ADT/DenseSet.h>
#include <llvm/ADT/Optional.h>
#include <llvm/ADT/SCCIterator.h>
#include <llvm/Analysis/BasicAliasAnalysis.h>
#include <llvm/Analysis/LoopInfo.h>
#include <llvm/Analysis/MemoryLocation.h>
#include <llvm/IR/CFG.h>
#include <llvm/IR/Constants.h>
#include <llvm/IR/Instructions.h>

#include <algorithm>
#include <list>
#include <stack>

namespace llvm {

void initializeClosureIRPassPass(PassRegistry&);

} // End namespace llvm

namespace llosl {

class ClosureIRPass::Context {
public:

    Context(llvm::Function&, llvm::LoopInfo&, llvm::AAResults&);

    llvm::LoopInfo&       loop_info()       { return d_loop_info; }
    const llvm::LoopInfo& loop_info() const { return d_loop_info; }

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
    Return                    *createReturn(llvm::ReturnInst&);

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
    llvm::LoopInfo& d_loop_info;
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

ClosureIRPass::Context::Context(llvm::Function& function, llvm::LoopInfo& loop_info, llvm::AAResults& aa)
: d_ll_context(function.getParent()->getContext())
, d_module(*function.getParent())
, d_function(function)
, d_loop_info(loop_info)
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

Return *
ClosureIRPass::Context::createReturn(llvm::ReturnInst& return_instruction) {
    auto block = getBlock();
    auto instruction = new Return(return_instruction, block);
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
    Context context(F,
        getAnalysis<llvm::LoopInfoWrapperPass>().getLoopInfo(),
        getAnalysis<llvm::AAResultsWrapperPass>().getAAResults());

    // Determine closure storage:
    std::for_each(
        context.blocks_begin(), context.blocks_end(),
        [this, &context](const auto& scc) -> void {
            if (context.loop_info().isLoopHeader(*scc.rbegin())) {
                return;
            }

            std::for_each(
                scc.rbegin(), scc.rend(),
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
            if (context.loop_info().isLoopHeader(*scc.rbegin())) {
                return;
            }

            std::for_each(
                scc.rbegin(), scc.rend(),
                [this, &context](const auto block) -> void {
                    context.beginBlock(*block);

                    std::for_each(
                        block->begin(), block->end(),
                        [this, &context](auto& ll_instruction) -> void {
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
                            case llvm::Instruction::Ret: {
                                auto return_instruction = llvm::cast<llvm::ReturnInst>(&ll_instruction);
                                context.createReturn(*return_instruction);
                            } break;
                            default:
                                break;
                            };
                        });

                    context.endBlock();
                });
        });

    {
        struct Frame {
            const llvm::BasicBlock *ll_block = nullptr;
            Block *block = nullptr;
            bool back = false;
        };

        std::stack<Frame> stack;

        enum class Color {
            White, Grey, Black
        };

        llvm::DenseMap<const llvm::BasicBlock *, Color> color;

        std::for_each(
            F.begin(), F.end(),
            [&context, &stack, &color](const auto& ll_block) -> void {
                color[&ll_block] = Color::White;

                if (llvm::pred_begin(&ll_block) == llvm::pred_end(&ll_block)) {
                    auto value = context.findValue(&ll_block);
                    stack.push({ &ll_block, value ? llvm::cast<Block>(*value) : nullptr, false });
                }
            });

        while (!stack.empty()) {
            auto ll_block = stack.top().ll_block;
            auto block    = stack.top().block;
            auto back     = stack.top().back;
            stack.pop();

            if (!back && color[ll_block] == Color::White) {
                color[ll_block] = Color::Grey;

                auto terminator = ll_block->getTerminator();

                for (unsigned i = 0, n = terminator->getNumSuccessors(); i < n; ++i) {
                    auto ll_succ = terminator->getSuccessor(i);

                    auto value = context.findValue(ll_succ);
                    Block *succ = value ? llvm::cast<Block>(*value) : nullptr;

                    if (succ) {
                        block->insertSuccessor(succ, terminator, i);
                    }

                    if (color[ll_succ] == Color::White) {
                        stack.push({ ll_succ, succ ? succ : block, false });
                    }
                }
            }
            else if (back) {
                color[ll_block] = Color::Black;
            }
        }
    }

    auto function = context.endFunction();

    d_closure_function = std::move(function);

    return true;
}

void ClosureIRPass::getAnalysisUsage(llvm::AnalysisUsage &AU) const {
    AU.setPreservesAll();
    AU.addRequired<llvm::LoopInfoWrapperPass>();
    AU.addRequired<llvm::AAResultsWrapperPass>();
}

} // End namespace llosl

using llosl::ClosureIRPass;
using namespace llvm;

INITIALIZE_PASS_BEGIN(ClosureIRPass, "llosl-closure-ir",
                      "ClosureIR", false, false)
INITIALIZE_PASS_DEPENDENCY(AAResultsWrapperPass)
INITIALIZE_PASS_END(ClosureIRPass, "llosl-closure-ir",
                    "ClosureIR", false, false)
