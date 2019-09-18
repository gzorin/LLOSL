#include <llosl/IR/Block.h>
#include <llosl/IR/ClosureFunction.h>
#include <llosl/IR/ClosureIRPass.h>
#include <llosl/IR/PathInfoPass.h>

#include <llvm/ADT/DenseMap.h>
#include <llvm/IR/Value.h>

#include <iostream>
#include <list>
#include <stack>

namespace llvm {

void initializePathInfoPassPass(PassRegistry&);

} // End namespace llvm

namespace llosl {

PathInfo::PathInfo(std::shared_ptr<const ClosureFunction> ir)
: d_ir(ir) {
}

llvm::Optional<const PathInfo::BlockInfo *>
PathInfo::getInfoForBlock(const Block *block) const {
    auto it = d_block_info.find(block);
    if (it == d_block_info.end()) {
        return llvm::Optional<const PathInfo::BlockInfo *>();
    }

    return llvm::Optional<const PathInfo::BlockInfo *>(&it->second);
}

llvm::Optional<unsigned>
PathInfo::getPathCountForBlock(const Block *block) const {
    auto block_info = getInfoForBlock(block);
    if (!block_info) {
        return llvm::Optional<unsigned>();
    }

    return llvm::Optional<unsigned>((*block_info)->path_count);
}

llvm::Optional<const PathInfo::EdgeIDMap *>
PathInfo::getEdgeIDsForBlock(const Block *block) const {
    auto block_info = getInfoForBlock(block);
    if (!block_info) {
        return llvm::Optional<const EdgeIDMap *>();
    }

    return llvm::Optional<const EdgeIDMap *>(&(*block_info)->edge_id);
}

llvm::Optional<unsigned>
PathInfo::getEdgeIDForBlocks(const Block *block, const Block *succ) const {
    auto edge_ids = getEdgeIDsForBlock(block);
    if (!edge_ids) {
        return llvm::Optional<unsigned>();
    }

    auto edge_id_map = *edge_ids;
    auto it = edge_id_map->find(succ);
    if (it == edge_id_map->end()) {
        return llvm::Optional<unsigned>();
    }

    return llvm::Optional<unsigned>(it->second);
}

void
PathInfo::insertLeaf(const Block *block) {
    auto& block_info = d_block_info[block];
    block_info.path_count = 1;
}

void
PathInfo::insertEdge(const Block *block, const Block *succ) {
    auto& block_info = d_block_info[block];

    auto jt = d_block_info.find(succ);
    assert(jt != d_block_info.end());
    auto& succ_info = jt->second;

    block_info.edge_id[succ] = block_info.path_count;
    block_info.path_count += succ_info.path_count;
}

PathInfoPass::PathInfoPass() : FunctionPass(ID) {
    llvm::initializePathInfoPassPass(*llvm::PassRegistry::getPassRegistry());
}

char PathInfoPass::ID = 0;

llvm::FunctionPass *createPathInfoPass() {
    return new PathInfoPass();
}

bool PathInfoPass::runOnFunction(llvm::Function &F) {
    auto function = getAnalysis<ClosureIRPass>().getIR();

    auto path_info = std::make_unique<PathInfo>(function);

    // Reverse topological sort:
    std::list<const Block *> blocks;

    struct Frame {
        const Block *block = nullptr;
        bool back = false;
    };

    std::stack<Frame> stack;

    enum class Color {
        White, Grey, Black
    };

    llvm::DenseMap<const Block *, Color> color;

    std::for_each(
        function->blocks_begin(), function->blocks_end(),
        [&stack, &color](const auto& block) -> void {
            color[&block] = Color::White;

            if (block.preds_begin() == block.preds_end()) {
                stack.push({ &block, false });
            }
        });

    while (!stack.empty()) {
        auto block    = stack.top().block;
        auto back     = stack.top().back;
        stack.pop();

        if (!back && color[block] == Color::White) {
            color[block] = Color::Grey;

            stack.push({ block, true });

            std::for_each(
                block->succs_begin(), block->succs_end(),
                [&stack, &color](const auto& succs) {
                    auto succ = succs.first;

                    if (color[succ] == Color::White) {
                        stack.push({ succ, false });
                    }
                });
        }
        else if (back) {
            color[block] = Color::Black;
            blocks.push_back(block);
        }
    }

    // Populate PathInfo:
    std::for_each(
        blocks.begin(), blocks.end(),
        [&path_info](auto block) -> void {
            // Leaf block:
            if (block->succs_begin() == block->succs_end()) {
                path_info->insertLeaf(block);
                return;
            }

            // Inner block:
            std::for_each(
                block->succs_begin(), block->succs_end(),
                [&path_info, block](const auto& succs) -> void {
                    auto succ = succs.first;
                    path_info->insertEdge(block, succ);
                });
        });

    d_path_info = std::move(path_info);

    return true;
}

void PathInfoPass::getAnalysisUsage(llvm::AnalysisUsage &AU) const {
    AU.setPreservesAll();
    AU.addRequired<ClosureIRPass>();
}

} // End namespace llosl

using llosl::PathInfoPass;
using namespace llvm;

INITIALIZE_PASS_BEGIN(PathInfoPass, "llosl-path-info",
                      "PathInfo", false, false)
INITIALIZE_PASS_DEPENDENCY(AAResultsWrapperPass)
INITIALIZE_PASS_END(PathInfoPass, "llosl-path-info",
                    "PathInfo", false, false)
