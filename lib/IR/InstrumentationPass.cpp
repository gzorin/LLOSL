#include <llosl/IR/InstrumentationPass.h>

#include <llvm/ADT/DenseMap.h>
#include <llvm/ADT/SCCIterator.h>
#include <llvm/IR/CFG.h>
#include <llvm/IR/Constants.h>
#include <llvm/IR/DerivedTypes.h>
#include <llvm/IR/Function.h>
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/Instructions.h>
#include <llvm/IR/Module.h>
#include <llvm/Transforms/Utils/BasicBlockUtils.h>

#include <algorithm>
#include <iostream>
#include <numeric>

namespace llvm {

void initializeInstrumentationPassPass(PassRegistry&);

} // End namespace llvm

namespace llosl {

InstrumentationPass::InstrumentationPass() : FunctionPass(ID) {
    llvm::initializeInstrumentationPassPass(*llvm::PassRegistry::getPassRegistry());
}

char InstrumentationPass::ID = 0;

llvm::FunctionPass *createInstrumentationPass() {
    return new InstrumentationPass();
}

bool InstrumentationPass::runOnFunction(llvm::Function &F) {
    auto module = F.getParent();
    auto& ll_context = module->getContext();

    struct BlockInfo {
        unsigned num_paths = 0;
        llvm::DenseMap<const llvm::BasicBlock *, unsigned> edge_id;
    };

    llvm::DenseMap<llvm::BasicBlock *, BlockInfo> block_infos;

    auto scc_begin = llvm::scc_iterator<llvm::Function *>::begin(&F),
         scc_end   = llvm::scc_iterator<llvm::Function *>::end(&F);

    std::for_each(
        scc_begin, scc_end,
        [&block_infos](const auto& scc) -> void {
            std::for_each(
                scc.begin(), scc.end(),
                [&block_infos](const auto block) -> void {
                    auto& block_info = block_infos[block];

                    auto terminator = block->getTerminator();

                    if (llvm::isa<llvm::ReturnInst>(terminator)) {
                        block_info.num_paths = 1;
                        return;
                    }

                    block_info.num_paths = 0;

                    std::for_each(
                        llvm::succ_begin(block), llvm::succ_end(block),
                        [&block_infos, block, &block_info](auto succ) -> void {
                            block_info.edge_id[succ] = block_info.num_paths;
                            block_info.num_paths += block_infos[succ].num_paths;
                        });
                });
        });

    auto int16_type = llvm::Type::getInt16Ty(ll_context);

    auto function_type = F.getFunctionType();
    auto instrumented_function_type = llvm::FunctionType::get(
        int16_type, function_type->params(), false);
    auto instrumented_function = llvm::Function::Create(
        instrumented_function_type, llvm::GlobalValue::ExternalLinkage, "", module);

    instrumented_function->takeName(&F);

    // Replace uses of arguments:
    std::accumulate(
        F.arg_begin(), F.arg_end(),
        instrumented_function->arg_begin(),
        [](llvm::Argument *instrumented_function_arg, llvm::Argument& function_arg) -> llvm::Argument * {
            function_arg.replaceAllUsesWith(instrumented_function_arg++);
            return instrumented_function_arg;
        });

    //
    auto path_id_init_block = llvm::BasicBlock::Create(ll_context, "llosl.init.path_id", instrumented_function);
    llvm::IRBuilder<> builder(ll_context);
    builder.SetInsertPoint(path_id_init_block);

    auto path_id = builder.CreateAlloca(int16_type, 0, "llosl.path_id");
    auto init_path_id = builder.CreateStore(
        llvm::ConstantInt::get(int16_type, 0, false), path_id);

    builder.CreateBr(&F.getEntryBlock());

    // Move blocks:
    for (auto it = F.begin(), it_end = F.end(); it != it_end; ) {
        auto block = &*it++;
        block->removeFromParent();
        block->insertInto(instrumented_function);
    }

    std::for_each(
        block_infos.begin(), block_infos.end(),
        [&ll_context, instrumented_function, int16_type, path_id](const auto& tmp) {
            auto block = tmp.first;
            const auto& block_info = tmp.second;

            auto current_path_id = new llvm::LoadInst(path_id, "llosl.cur.path_id.");
            auto it = block->getFirstInsertionPt();
            block->getInstList().insert(it, current_path_id);

            auto terminator = block->getTerminator();

            if (llvm::isa<llvm::ReturnInst>(terminator)) {
                auto return_terminator = llvm::ReturnInst::Create(ll_context, current_path_id);
                llvm::ReplaceInstWithInst(terminator, return_terminator);
                return;
            }

            for (unsigned i = 0, n = terminator->getNumSuccessors(); i < n; ++i) {
                auto succ = terminator->getSuccessor(i);
                auto it = block_info.edge_id.find(succ);
                assert(it != block_info.edge_id.end());

                auto edge_id = it->second;

                auto incr_block = llvm::BasicBlock::Create(ll_context, "llosl.incr.path_id.", instrumented_function);

                llvm::IRBuilder<> builder(ll_context);
                builder.SetInsertPoint(incr_block);

                auto add_path_id = builder.CreateAdd(
                    current_path_id, llvm::ConstantInt::get(int16_type, edge_id, false));
                builder.CreateStore(add_path_id, path_id);
                builder.CreateBr(succ);

                terminator->setSuccessor(i, incr_block);
            }
        });

    return true;
}

} // End namespace llosl

using llosl::InstrumentationPass;
using namespace llvm;

INITIALIZE_PASS_BEGIN(InstrumentationPass, "llosl-instrumentation",
                      "Instrumentation", false, false)
INITIALIZE_PASS_DEPENDENCY(AAResultsWrapperPass)
INITIALIZE_PASS_END(InstrumentationPass, "llosl-instrumentation",
                    "Instrumentation", false, false)
