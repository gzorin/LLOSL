#include <llosl/IR/ClosureFunction.h>
#include <llosl/IR/ClosureIRPass.h>
#include <llosl/IR/PathInfoPass.h>
#include <llosl/IR/InstrumentationPass.h>

#include <llvm/ADT/DenseMap.h>
#include <llvm/IR/CFG.h>
#include <llvm/IR/Constants.h>
#include <llvm/IR/DerivedTypes.h>
#include <llvm/IR/Function.h>
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/Instructions.h>
#include <llvm/IR/Module.h>
#include <llvm/Transforms/Utils/BasicBlockUtils.h>

#include <algorithm>
#include <numeric>

namespace llosl {

llvm::Function *
InstrumentFunctionForPathId(llvm::Function& F, const PathInfo& path_info, llvm::StringRef name) {
    auto module = F.getParent();
    auto& ll_context = module->getContext();

    auto int16_type = llvm::Type::getInt16Ty(ll_context);

    auto instrumented_function = &F;

    //
    auto entry_block = &instrumented_function->getEntryBlock();

    auto path_id_init_block = llvm::BasicBlock::Create(ll_context, "llosl.init.path_id", instrumented_function, entry_block);

    llvm::IRBuilder<> builder(ll_context);
    builder.SetInsertPoint(path_id_init_block);

    auto path_id = builder.CreateAlloca(int16_type, 0, "llosl.path_id");
    auto init_path_id = builder.CreateStore(
        llvm::ConstantInt::get(int16_type, 0, false), path_id);

    builder.CreateBr(entry_block);

    // Instrument the blocks:
    auto function = path_info.getIR();

    std::for_each(
        function->blocks_begin(), function->blocks_end(),
        [&path_info, &ll_context, instrumented_function, path_id](auto& block) -> void {
            auto ll_block = const_cast<llvm::BasicBlock *>(llvm::cast<llvm::BasicBlock>(block.getLLValue())); // Bah....

            // Load the current value of `path_id` for this block:
            auto current_path_id = new llvm::LoadInst(path_id, "llosl.cur.path_id.");
            auto it = ll_block->getFirstInsertionPt();
            ll_block->getInstList().insert(it, current_path_id);

            // A block that returns from the function should just return the value of `path_id`:
            if (block.succs_begin() == block.succs_end()) {
                auto terminator = ll_block->getTerminator();
    //          assert(llvm::isa<llvm::ReturnInst>(terminator));

                auto return_terminator = llvm::ReturnInst::Create(ll_context, current_path_id);
                llvm::ReplaceInstWithInst(terminator, return_terminator);

                return;
            }

            // Otherwise, insert a block that increments `path_id` by `edge_id`:
            std::for_each(
                block.succs_begin(), block.succs_end(),
                [&ll_context, &path_info, instrumented_function, path_id, current_path_id, &block](const auto& succs) -> void {
                    auto succ = succs.first;
                    const auto& terminators = succs.second;

                    auto ll_succ = const_cast<llvm::BasicBlock *>(llvm::cast<llvm::BasicBlock>(succ->getLLValue())); // Bah....

                    auto edge_id = path_info.getEdgeIDForBlocks(&block, succ);
                    assert(edge_id);

                    auto incr_block = llvm::BasicBlock::Create(ll_context, "llosl.incr.path_id.", instrumented_function);

                    llvm::IRBuilder<> builder(ll_context);
                    builder.SetInsertPoint(incr_block);

                    auto int16_type = llvm::Type::getInt16Ty(ll_context);

                    auto add_path_id = builder.CreateAdd(
                        current_path_id, llvm::ConstantInt::get(int16_type, *edge_id, false));
                    builder.CreateStore(add_path_id, path_id);
                    builder.CreateBr(ll_succ);

                    // Replace
                    std::for_each(
                        terminators.begin(), terminators.end(),
                        [ll_succ, incr_block](auto tmp) -> void {
                            auto terminator = const_cast<llvm::TerminatorInst *>(tmp.first);
                            auto successor_index = tmp.second;
                            assert(terminator->getSuccessor(successor_index) == ll_succ);

                            terminator->setSuccessor(successor_index, incr_block);
                        });
                });
        });

    return instrumented_function;
}

} // End namespace llosl