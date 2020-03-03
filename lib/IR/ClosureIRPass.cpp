#include <llosl/IR/ClosureFunction.h>
#include <llosl/IR/ClosureIRPass.h>
#include <llosl/IR/Instruction.h>

#include <llvm/ADT/DenseMap.h>
#include <llvm/ADT/DenseSet.h>
#include <llvm/ADT/Optional.h>
#include <llvm/ADT/SCCIterator.h>
#include <llvm/Analysis/BasicAliasAnalysis.h>
#include <llvm/Analysis/MemoryLocation.h>
#include <llvm/IR/CFG.h>
#include <llvm/IR/Constants.h>
#include <llvm/IR/Instructions.h>

#include <algorithm>
#include <list>
#include <map>
#include <set>
#include <stack>

namespace llvm {

void initializeClosureIRPassPass(PassRegistry &);

} // End namespace llvm

namespace llosl {

class ClosureIRPass::Context {
public:
    Context(llvm::Function &, llvm::AAResults &);

    llvm::Function *llosl_closure_Ci_annotation      = nullptr,
                   *llosl_closure_output_annotation  = nullptr,
                   *llosl_closure_storage_annotation = nullptr, *osl_add_closure_closure = nullptr,
                   *osl_mul_closure_color = nullptr, *osl_mul_closure_float = nullptr,
                   *osl_allocate_closure_component          = nullptr,
                   *osl_allocate_weighted_closure_component = nullptr;

    ClosureFunction *getFunction() const { return d_closure_function.get(); }

    using SortedBlocks = std::list<std::vector<llvm::BasicBlock *>>;
    const SortedBlocks &         getBlocksInTopologicalOrder() const { return d_sorted_blocks; };
    SortedBlocks::const_iterator blocks_begin() const { return d_sorted_blocks.begin(); }
    SortedBlocks::const_iterator blocks_end() const { return d_sorted_blocks.end(); }

    //
    unsigned                 insertClosureStorage(llvm::MemoryLocation);
    unsigned                 insertClosureOutput(llvm::MemoryLocation);
    unsigned                 insertClosureCi(llvm::MemoryLocation);
    llvm::Optional<unsigned> findClosureStorage(llvm::MemoryLocation) const;

    //
    void                             beginFunction();
    std::unique_ptr<ClosureFunction> endFunction();

    ClosureFunction *      function() { return d_closure_function.get(); };
    const ClosureFunction *function() const { return d_closure_function.get(); };

    //
    void beginBlock(llvm::BasicBlock &);
    void endBlock();

    ClosureBlock *      block() { return d_block; };
    const ClosureBlock *block() const { return d_block; };

    //
    Reference *                createReference(const llvm::CallInst &);
    AllocateComponent *        createAllocateComponent(const llvm::CallInst &);
    AllocateWeightedComponent *createAllocateWeightedComponent(const llvm::CallInst &);
    AddClosureClosure *        createAddClosureClosure(const llvm::CallInst &);
    MulClosureColor *          createMulClosureColor(const llvm::CallInst &);
    MulClosureFloat *          createMulClosureFloat(const llvm::CallInst &);
    Load *                     createLoad(const llvm::LoadInst &);
    Store *                    createStore(const llvm::StoreInst &);
    Cast *                     createCast(const llvm::CastInst &);
    PHI *                      createPHI(const llvm::PHINode &);
    NonClosureRegion *         createNonClosureRegion(std::list<llvm::BasicBlock *> &&);
    Return *                   createReturn(llvm::ReturnInst &);

    //
    void                    insertValue(Value *);
    void                    insertValue(const llvm::Value *, Value *);
    llvm::Optional<Value *> findValue(const llvm::Value *);
    llvm::Optional<Block *> findBlock(const llvm::BasicBlock *);

private:
    enum class State { ClosureStorage, Function, Block, Final };

    ClosureBlock *getBlock();

    State d_state = State::ClosureStorage;

    llvm::LLVMContext &d_ll_context;
    llvm::Module &     d_module;
    llvm::Function &   d_function;
    llvm::AAResults &  d_aa;

    SortedBlocks d_sorted_blocks;

    // Memory locations that are known to refer to closures:
    mutable llvm::DenseMap<llvm::MemoryLocation, unsigned> d_closure_locations;
    unsigned                                               d_closure_storage_count = 0;
    std::set<unsigned>                                     d_closure_output_locations;
    std::optional<unsigned>                                d_closure_Ci_location;

    // The currently evolving function:
    std::unique_ptr<ClosureFunction> d_closure_function;

    // The block being processed:
    llvm::BasicBlock *d_ll_block = nullptr;
    ClosureBlock *    d_block    = nullptr;

    // Values that are known to represent closures:
    llvm::DenseMap<const llvm::Value *, Value *> d_value_map;

    Null *d_null = nullptr;
};

ClosureIRPass::Context::Context(llvm::Function &function, llvm::AAResults &aa)
    : d_ll_context(function.getParent()->getContext())
    , d_module(*function.getParent())
    , d_function(function)
    , d_aa(aa) {
    llosl_closure_Ci_annotation      = d_module.getFunction("llosl_closure_Ci_annotation");
    llosl_closure_output_annotation  = d_module.getFunction("llosl_closure_output_annotation");
    llosl_closure_storage_annotation = d_module.getFunction("llosl_closure_storage_annotation");
    osl_add_closure_closure          = d_module.getFunction("osl_add_closure_closure");
    osl_mul_closure_color            = d_module.getFunction("osl_mul_closure_color");
    osl_mul_closure_float            = d_module.getFunction("osl_mul_closure_float");
    osl_allocate_closure_component   = d_module.getFunction("osl_allocate_closure_component");
    osl_allocate_weighted_closure_component =
        d_module.getFunction("osl_allocate_weighted_closure_component");

    //
    std::copy(llvm::scc_iterator<llvm::Function *>::begin(&d_function),
              llvm::scc_iterator<llvm::Function *>::end(&d_function),
              std::back_inserter(d_sorted_blocks));

    std::reverse(d_sorted_blocks.begin(), d_sorted_blocks.end());
}

unsigned
ClosureIRPass::Context::insertClosureStorage(llvm::MemoryLocation location) {
    assert(d_state == State::ClosureStorage);

    auto tmp = d_closure_locations.insert({location, d_closure_storage_count++});
    assert(tmp.second);

    return tmp.first->second;
}

unsigned
ClosureIRPass::Context::insertClosureOutput(llvm::MemoryLocation location) {
    return *d_closure_output_locations.insert(insertClosureStorage(location)).first;
}

unsigned
ClosureIRPass::Context::insertClosureCi(llvm::MemoryLocation location) {
    assert(!d_closure_Ci_location);

    d_closure_Ci_location = insertClosureOutput(location);
    return *d_closure_Ci_location;
}

llvm::Optional<unsigned>
ClosureIRPass::Context::findClosureStorage(llvm::MemoryLocation location) const {
    // First try to find `location` in the map:
    auto it = d_closure_locations.find(location);

    // Even if `location` is not in the map, it might alias something that is:
    if (it == d_closure_locations.end()) {
        auto jt = std::find_if(d_closure_locations.begin(), d_closure_locations.end(),
                               [this, location](auto tmp) -> bool {
                                   auto result = d_aa.alias(location, tmp.first);
                                   return result == llvm::MustAlias || result == llvm::PartialAlias;
                               });
        if (jt != d_closure_locations.end()) {
            it = d_closure_locations.insert({location, jt->second}).first;
        }
    }

    if (it != d_closure_locations.end()) {
        return {it->second};
    }
    else {
        return llvm::Optional<unsigned>();
    }
}

void
ClosureIRPass::Context::beginFunction() {
    assert(d_state == State::ClosureStorage);

    d_closure_function = std::make_unique<ClosureFunction>(
        d_closure_storage_count, d_closure_output_locations, d_closure_Ci_location);

    d_state = State::Function;
}

std::unique_ptr<ClosureFunction>
ClosureIRPass::Context::endFunction() {
    assert(d_state == State::Function);

    d_state = State::Final;

    return std::move(d_closure_function);
}

void
ClosureIRPass::Context::beginBlock(llvm::BasicBlock &ll_block) {
    assert(d_state == State::Function);

    d_ll_block = &ll_block;

    d_state = State::Block;
}

void
ClosureIRPass::Context::endBlock() {
    assert(d_state == State::Block);

    d_ll_block = nullptr;
    d_block    = nullptr;

    d_state = State::Function;
}

ClosureBlock *
ClosureIRPass::Context::getBlock() {
    assert(d_state == State::Block);

    if (!d_block) {
        d_block = new ClosureBlock(*d_ll_block, d_closure_function.get());
        insertValue(d_block);
    }

    return d_block;
}

Reference *
ClosureIRPass::Context::createReference(const llvm::CallInst &call_instruction) {
    auto block = getBlock();

    auto storage = call_instruction.getArgOperand(0);

    llvm::MemoryLocation location(storage, 8);
    auto                 tmp = findClosureStorage(location);
    assert(tmp);

    auto instruction = new Reference(*storage, *tmp, block);
    insertValue(instruction);
    insertValue(&call_instruction, instruction);
    return instruction;
}

AllocateComponent *
ClosureIRPass::Context::createAllocateComponent(const llvm::CallInst &call_instruction) {
    auto block       = getBlock();
    auto instruction = new AllocateComponent(call_instruction, block);
    insertValue(instruction);
    return instruction;
}

AllocateWeightedComponent *
ClosureIRPass::Context::createAllocateWeightedComponent(const llvm::CallInst &call_instruction) {
    auto block       = getBlock();
    auto instruction = new AllocateWeightedComponent(call_instruction, block);
    insertValue(instruction);
    return instruction;
}

AddClosureClosure *
ClosureIRPass::Context::createAddClosureClosure(const llvm::CallInst &call_instruction) {
    auto block = getBlock();

    llvm::MemoryLocation lhs_location(call_instruction.getArgOperand(1), 8);
    auto                 lhs = findClosureStorage(lhs_location);
    assert(lhs);

    llvm::MemoryLocation rhs_location(call_instruction.getArgOperand(2), 8);
    auto                 rhs = findClosureStorage(rhs_location);
    assert(rhs);

    auto instruction = new AddClosureClosure(call_instruction, *lhs, *rhs, block);
    insertValue(instruction);
    return instruction;
}

MulClosureColor *
ClosureIRPass::Context::createMulClosureColor(const llvm::CallInst &call_instruction) {
    auto block = getBlock();

    llvm::MemoryLocation lhs_location(call_instruction.getArgOperand(1), 8);
    auto                 lhs = findClosureStorage(lhs_location);
    assert(lhs);

    auto instruction = new MulClosureColor(call_instruction, *lhs, block);
    insertValue(instruction);
    return instruction;
}

MulClosureFloat *
ClosureIRPass::Context::createMulClosureFloat(const llvm::CallInst &call_instruction) {
    auto block = getBlock();

    llvm::MemoryLocation lhs_location(call_instruction.getArgOperand(1), 8);
    auto                 lhs = findClosureStorage(lhs_location);
    assert(lhs);

    auto instruction = new MulClosureFloat(call_instruction, *lhs, block);
    insertValue(instruction);
    return instruction;
}

Load *
ClosureIRPass::Context::createLoad(const llvm::LoadInst &load_instruction) {
    auto location = llvm::MemoryLocation::get(&load_instruction);
    auto tmp      = findClosureStorage(location);
    if (!tmp) {
        return nullptr;
    }

    auto block       = getBlock();
    auto instruction = new Load(load_instruction, *tmp, block);
    insertValue(instruction);
    return instruction;
}

Store *
ClosureIRPass::Context::createStore(const llvm::StoreInst &store_instruction) {
    auto location = llvm::MemoryLocation::get(&store_instruction);
    auto tmp      = findClosureStorage(location);
    if (!tmp) {
        return nullptr;
    }

    auto value = store_instruction.getValueOperand();

    if (llvm::isa<llvm::Constant>(value) && !d_null) {
        auto block = getBlock();
        d_null     = new Null(*value, block);
        insertValue(d_null);
    }

    auto rhs = findValue(value);
    assert(rhs);

    auto block       = getBlock();
    auto instruction = new Store(store_instruction, **rhs, *tmp, block);
    insertValue(instruction);
    return instruction;
}

Cast *
ClosureIRPass::Context::createCast(const llvm::CastInst &cast_instruction) {
    auto rhs = findValue(cast_instruction.getOperand(0));
    if (!rhs) {
        return nullptr;
    }

    auto block       = getBlock();
    auto instruction = new Cast(cast_instruction, **rhs, block);
    insertValue(instruction);
    return instruction;
}

PHI *
ClosureIRPass::Context::createPHI(const llvm::PHINode &phi_node) {
    std::vector<Value *> operands;
    std::for_each(phi_node.op_begin(), phi_node.op_end(),
                  [this, &operands](const auto &ll_operand) -> void {
                      auto operand = findValue(ll_operand.get());
                      if (!operand) {
                          return;
                      }

                      operands.push_back(*operand);
                  });

    if (operands.empty()) {
        return nullptr;
    }

    auto block       = getBlock();
    auto instruction = new PHI(phi_node, operands, block);
    insertValue(instruction);
    return instruction;
}

NonClosureRegion *
ClosureIRPass::Context::createNonClosureRegion(std::list<llvm::BasicBlock *> &&ll_blocks) {
    auto closure_region = new NonClosureRegion(std::move(ll_blocks), d_closure_function.get());

    // TODO: associate each of `ll_blocks` with `closure_region`
    std::for_each(
        closure_region->blocks_begin(), closure_region->blocks_end(),
        [this, closure_region](auto ll_block) -> void { insertValue(ll_block, closure_region); });

    return closure_region;
}

Return *
ClosureIRPass::Context::createReturn(llvm::ReturnInst &return_instruction) {
    auto block       = getBlock();
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
    d_value_map.insert({ll_value, value});
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

llvm::Optional<Block *>
ClosureIRPass::Context::findBlock(const llvm::BasicBlock *ll_block) {
    auto value = findValue(ll_block);
    if (!value) {
        return llvm::Optional<Block *>();
    }

    assert(llvm::isa<Block>(*value));
    return llvm::Optional<Block *>(llvm::cast<Block>(*value));
}

ClosureIRPass::ClosureIRPass()
    : FunctionPass(ID) {
    llvm::initializeClosureIRPassPass(*llvm::PassRegistry::getPassRegistry());
}

char ClosureIRPass::ID = 0;

bool
ClosureIRPass::runOnFunction(llvm::Function &F) {
    Context context(F, getAnalysis<llvm::AAResultsWrapperPass>().getAAResults());

    enum class Color { Grey, Black };

    // Sort blocks topologically:
    llvm::DenseMap<llvm::BasicBlock *, Color> color;

    std::list<llvm::BasicBlock *> ll_blocks;

    std::for_each(F.begin(), F.end(), [&context, &color, &ll_blocks](auto &tmp) -> void {
        auto ll_block = &tmp;

        if (color.count(ll_block)) {
            return;
        }

        struct Frame {
            llvm::BasicBlock *ll_block = nullptr;
            bool              back     = false;
        };

        std::stack<Frame> stack;

        stack.push({ll_block, false});

        while (!stack.empty()) {
            auto [ll_block, back] = stack.top();
            stack.pop();

            if (back) {
                color[ll_block] = Color::Black;

                ll_blocks.push_front(ll_block);

                continue;
            }

            color[ll_block] = Color::Grey;

            stack.push({ll_block, true});

            std::for_each(
                ll_block->begin(), ll_block->end(), [&context](const auto &ll_instruction) -> void {
                    auto call_instruction = llvm::dyn_cast<llvm::CallInst>(&ll_instruction);
                    if (!call_instruction) {
                        return;
                    }

                    auto called_function = call_instruction->getCalledValue();

                    if (called_function != context.llosl_closure_Ci_annotation &&
                        called_function != context.llosl_closure_output_annotation &&
                        called_function != context.llosl_closure_storage_annotation) {
                        return;
                    }

                    auto storage = call_instruction->getOperand(0);

                    if (called_function == context.llosl_closure_Ci_annotation) {
                        context.insertClosureCi(llvm::MemoryLocation(storage, 8));
                    }
                    else if (called_function == context.llosl_closure_output_annotation) {
                        context.insertClosureOutput(llvm::MemoryLocation(storage, 8));
                    }
                    else {
                        context.insertClosureStorage(llvm::MemoryLocation(
                            storage,
                            llvm::cast<llvm::ConstantInt>(call_instruction->getArgOperand(1))
                                    ->getSExtValue() *
                                8));
                    }
                });

            for (auto it = llvm::succ_begin(ll_block), it_end = llvm::succ_end(ll_block);
                 it != it_end; ++it) {
                auto ll_succ = *it;

                if (color.count(ll_succ)) {
                    continue;
                }

                stack.push({ll_succ, false});
            }
        }
    });

    //
    context.beginFunction();

    // Create `ClosureBlocks`s:
    std::for_each(ll_blocks.begin(), ll_blocks.end(), [&context](auto ll_block) -> void {
        context.beginBlock(*ll_block);

        std::for_each(ll_block->begin(), ll_block->end(), [&context](auto &ll_instruction) -> void {
            switch (ll_instruction.getOpcode()) {
            case llvm::Instruction::Call: {
                auto call_instruction = llvm::cast<llvm::CallInst>(&ll_instruction);
                auto called_function  = call_instruction->getCalledValue();

                if (called_function == context.llosl_closure_Ci_annotation ||
                    called_function == context.llosl_closure_output_annotation ||
                    called_function == context.llosl_closure_storage_annotation) {
                    context.createReference(*call_instruction);
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

    // Create `NonClosureRegion`s:
    color.clear();

    std::for_each(F.begin(), F.end(), [&context, &color](auto &tmp) -> void {
        auto ll_block = &tmp;

        auto block = context.findBlock(ll_block);
        if (block) {
            return;
        }

        if (color.count(ll_block)) {
            return;
        }

        struct Frame {
            llvm::BasicBlock *ll_block = nullptr;
            bool              back     = false;
        };

        std::stack<Frame> stack;

        std::list<llvm::BasicBlock *> nc_ll_blocks;

        stack.push({ll_block, false});

        while (!stack.empty()) {
            auto [ll_block, back] = stack.top();
            stack.pop();

            if (back) {
                color[ll_block] = Color::Black;

                auto block = context.findBlock(ll_block);
                if (!block) {
                    nc_ll_blocks.push_front(ll_block);
                }

                continue;
            }

            color[ll_block] = Color::Grey;

            stack.push({ll_block, true});

            for (auto it = llvm::succ_begin(ll_block), it_end = llvm::succ_end(ll_block);
                 it != it_end; ++it) {
                auto ll_succ = *it;

                // Do not visit closure blocks:
                auto succ = context.findBlock(ll_succ);
                if (succ) {
                    continue;
                }

                if (color.count(ll_succ)) {
                    continue;
                }

                stack.push({ll_succ, false});
            }
        }

        if (!nc_ll_blocks.empty()) {
            context.createNonClosureRegion(std::move(nc_ll_blocks));
        }
    });

    // Connect all of the blocks:
    color.clear();

    std::for_each(F.begin(), F.end(), [&context, &color](auto &tmp) -> void {
        auto ll_block = &tmp;

        if (color.count(ll_block)) {
            return;
        }

        struct Frame {
            llvm::BasicBlock *ll_block = nullptr;
            bool              back     = false;
        };

        std::stack<Frame> stack;

        stack.push({ll_block, false});

        while (!stack.empty()) {
            auto [ll_block, back] = stack.top();
            stack.pop();

            if (back) {
                color[ll_block] = Color::Black;

                continue;
            }

            color[ll_block] = Color::Grey;

            stack.push({ll_block, true});

            auto block = context.findBlock(ll_block);
            assert(block);

            for (auto it = llvm::succ_begin(ll_block), it_end = llvm::succ_end(ll_block);
                 it != it_end; ++it) {
                auto ll_succ = *it;

                auto succ = context.findBlock(ll_succ);
                assert(block);

                if (block != succ) {
                    (*block)->insertSuccessor(*succ, {ll_block, ll_succ});
                }

                if (color.count(ll_succ)) {
                    continue;
                }

                stack.push({ll_succ, false});
            }
        }
    });

    auto function = context.endFunction();

    d_closure_function = std::move(function);

    return true;
}

void
ClosureIRPass::getAnalysisUsage(llvm::AnalysisUsage &AU) const {
    AU.setPreservesAll();
    AU.addRequired<llvm::AAResultsWrapperPass>();
}

} // End namespace llosl

using llosl::ClosureIRPass;
using namespace llvm;

INITIALIZE_PASS_BEGIN(ClosureIRPass, "llosl-closure-ir", "ClosureIR", false, false)
INITIALIZE_PASS_DEPENDENCY(AAResultsWrapperPass)
INITIALIZE_PASS_END(ClosureIRPass, "llosl-closure-ir", "ClosureIR", false, false)
