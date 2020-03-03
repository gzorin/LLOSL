#include <llosl/IR/ClosureFunction.h>
#include <llosl/IR/ClosureIRPass.h>
#include <llosl/IR/InstrumentationPass.h>
#include <llosl/IR/PathInfoPass.h>

#include <llvm/ADT/DenseMap.h>
#include <llvm/Analysis/CFG.h>
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
#include <stack>

namespace llosl {

llvm::Function *
InstrumentFunctionForPathId(llvm::Function &F, const PathInfo &path_info, llvm::StringRef name) {
    auto  module     = F.getParent();
    auto &ll_context = module->getContext();

    auto int16_type = llvm::Type::getInt16Ty(ll_context);

    auto instrumented_function = &F;

    //
    auto entry_block = &instrumented_function->getEntryBlock();

    auto path_id_init_block = llvm::BasicBlock::Create(ll_context, "llosl.init.path_id",
                                                       instrumented_function, entry_block);

    llvm::IRBuilder<> builder(ll_context);
    builder.SetInsertPoint(path_id_init_block);

    auto path_id      = builder.CreateAlloca(int16_type, 0, "llosl.path_id");
    auto init_path_id = builder.CreateStore(llvm::ConstantInt::get(int16_type, 0, false), path_id);

    builder.CreateBr(entry_block);

    // Instrument the blocks:
    auto function = path_info.getIR();

    std::for_each(
        function->blocks_begin(), function->blocks_end(),
        [&path_info, &ll_context, instrumented_function, path_id](auto &block) -> void {
            // Handle blocks that return from the function:
            if (block.succs_begin() == block.succs_end()) {
                auto ll_block =
                    llvm::cast<llvm::BasicBlock>(llvm::cast<ClosureBlock>(block).getLLBlock());

                // Load the current value of `path_id` for this block:
                auto current_path_id = new llvm::LoadInst(path_id);
                auto it              = ll_block->getFirstInsertionPt();
                ll_block->getInstList().insert(it, current_path_id);

                auto terminator = ll_block->getTerminator();
                assert(llvm::isa<llvm::ReturnInst>(terminator));

                llvm::ReplaceInstWithInst(terminator,
                                          llvm::ReturnInst::Create(ll_context, current_path_id));

                return;
            }

            // Otherwise, insert a block that increments `path_id` by `edge_id`:
            std::for_each(
                block.succs_begin(), block.succs_end(),
                [&ll_context, &path_info, instrumented_function, path_id,
                 &block](auto &tmp) -> void {
                    auto &[succ, edges] = tmp;

                    //
                    auto edge_id = path_info.getEdgeIDForBlocks(&block, succ);
                    assert(edge_id);

                    std::for_each(
                        edges.begin(), edges.end(),
                        [&ll_context, instrumented_function, path_id, edge_id](auto edge) -> void {
                            auto incr_block =
                                llvm::BasicBlock::Create(ll_context, "", instrumented_function);

                            llvm::IRBuilder<> builder(incr_block);

                            auto int16_type = llvm::Type::getInt16Ty(ll_context);

                            auto add_path_id = builder.CreateAdd(
                                builder.CreateLoad(path_id),
                                llvm::ConstantInt::get(int16_type, *edge_id, false));
                            builder.CreateStore(add_path_id, path_id);
                            builder.CreateBr(edge.to);

                            edge.from->getTerminator()->setSuccessor(
                                llvm::GetSuccessorNumber(edge.from, edge.to), incr_block);
                        });
                });
        });

    return instrumented_function;
}

} // End namespace llosl