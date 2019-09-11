#include <llosl/IR/ClosureIRPass.h>
#include <llosl/IR/ClosureFunction.h>

#include <llvm/ADT/Optional.h>
#include <llvm/ADT/SCCIterator.h>
#include <llvm/Analysis/BasicAliasAnalysis.h>
#include <llvm/Analysis/MemoryLocation.h>
#include <llvm/IR/CFG.h>
#include <llvm/IR/Constants.h>
#include <llvm/IR/Instructions.h>

#include <algorithm>
#include <iostream>

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

    llvm::GetElementPtrInst *Ci() const { return d_Ci.get(); }

    using SortedBlocks = std::vector<std::vector<llvm::BasicBlock *> >;
    const SortedBlocks& getBlocksInTopologicalOrder() const;

    llvm::Optional<unsigned> findClosureStorage(llvm::MemoryLocation) const;
    unsigned insertClosureStorage(llvm::MemoryLocation);

private:

    llvm::LLVMContext& d_ll_context;
    llvm::Module& d_module;
    llvm::Function& d_function;
    llvm::AAResults& d_aa;

    mutable std::unique_ptr<const SortedBlocks> d_blocks_in_topological_order;

    // Values that are known to represent closures:
    llvm::DenseSet<llvm::Value *> d_closure_values;

    // Memory locations that are known to refer to closures:
    mutable llvm::DenseMap<llvm::MemoryLocation, unsigned> d_closure_locations;
    unsigned d_closure_storage_count = 0;

    std::unique_ptr<llvm::GetElementPtrInst> d_Ci;
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

    auto globals = d_function.arg_begin();

    d_Ci.reset(
        llvm::GetElementPtrInst::Create(
            nullptr,
            globals, { llvm::ConstantInt::get(llvm::Type::getInt64Ty(d_ll_context),  0, true),
	                   llvm::ConstantInt::get(llvm::Type::getInt32Ty(d_ll_context), 20, true)  }));

    insertClosureStorage(llvm::MemoryLocation(d_Ci.get(), 1));
}

const ClosureIRPass::Context::SortedBlocks&
ClosureIRPass::Context::getBlocksInTopologicalOrder() const {
    if (d_blocks_in_topological_order) {
        return *d_blocks_in_topological_order;
    }

    SortedBlocks blocks;

    std::copy(
        llvm::scc_iterator<llvm::Function *>::begin(&d_function), llvm::scc_iterator<llvm::Function *>::end(&d_function),
        std::back_inserter(blocks));

    d_blocks_in_topological_order = std::move(
        std::make_unique<const SortedBlocks>(
            std::move(blocks)));

    return *d_blocks_in_topological_order;
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

unsigned
ClosureIRPass::Context::insertClosureStorage(llvm::MemoryLocation location) {
    auto tmp = d_closure_locations.insert({ location, d_closure_storage_count++});
    assert(tmp.second);

    return tmp.first->second;
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

    auto context = std::make_unique<Context>(
        F, getAnalysis<llvm::AAResultsWrapperPass>().getAAResults());

    const auto& sccs = context->getBlocksInTopologicalOrder();

    std::for_each(
        sccs.rbegin(), sccs.rend(),
        [this, &context](const auto& scc) -> void {
            std::for_each(
                scc.begin(), scc.end(),
                [this, &context](const auto block) -> void {
                    std::for_each(
                        block->begin(), block->end(),
                        [this, &context](const auto& instruction) -> void {
                            auto call_instruction = llvm::dyn_cast<llvm::CallInst>(&instruction);
                            if (!call_instruction) {
                                return;
                            }

                            auto called_function = call_instruction->getCalledValue();

                            if (called_function != context->osl_init_closure_storage) {
                                return;
                            }

                            auto storage = call_instruction->getArgOperand(1);
                            auto cast_instruction = llvm::dyn_cast<llvm::CastInst>(storage);
                            assert(cast_instruction);

                            storage = cast_instruction->getOperand(0);
                            auto alloca_instruction = llvm::dyn_cast<llvm::AllocaInst>(storage);
                            assert(alloca_instruction);

                            context->insertClosureStorage(llvm::MemoryLocation(alloca_instruction, 1));
                        });
                });
        });

    std::for_each(
        sccs.rbegin(), sccs.rend(),
        [this, &context](const auto& scc) -> void {
            std::for_each(
                scc.begin(), scc.end(),
                [this, &context](const auto block) -> void {
                    std::for_each(
                        block->begin(), block->end(),
                        [this, &context](const auto& instruction) -> void {
                            switch (instruction.getOpcode()) {
                            case llvm::Instruction::Store: {
                                auto location = llvm::MemoryLocation::get(&instruction);
                                auto tmp = context->findClosureStorage(location);

                                if (tmp) {
                                    instruction.dump();
                                }
                            } break;
                            default:
                                break;
                            };
                        });
                });
        });

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
