#include <llosl/IR/Block.h>
#include <llosl/IR/ClosureFunction.h>

#include <llvm/IR/BasicBlock.h>
#include <llvm/IR/InstrTypes.h>
#include <llvm/Support/raw_os_ostream.h>

namespace llosl {

//
Block::Block(Value::ValueKind kind, ClosureFunction *function)
: Value(kind) {
    setParent(function);
}

Block::~Block() {
}

void
Block::setParent(ClosureFunction *function) {
    if (d_function == function) {
        return;
    }

    if (d_function) {
        d_function->getBlockList().remove(*this);
    }

    d_function = function;

    if (d_function) {
        d_function->getBlockList().push_back(this);
    }
}

void
Block::insertSuccessor(Block *block, Edge edge) {
    d_successors[block].push_back(edge);
    block->d_predecessors[this].push_back(edge);
}

void
Block::eraseSuccessor(Block *block) {
    auto it = d_successors.find(block);
    if (it == d_successors.end()) {
        return;
    }

    d_successors.erase(block);
    block->d_predecessors.erase(this);
}

//
ClosureBlock::ClosureBlock(llvm::BasicBlock& ll_block, ClosureFunction *function)
: Block(Value::ValueKind::ClosureBlock, function)
, d_ll_block(ll_block) {
}

void
ClosureBlock::dropAllReferences() {
    std::for_each(
        d_insts.begin(), d_insts.end(),
        [](auto& instruction) -> void {
            instruction.dropAllReferences();
        });
}

const llvm::Value *
ClosureBlock::getLLValue() const {
    return &d_ll_block;
}

void
ClosureBlock::dump() const {
    llvm::errs() << "ClosureBlock: " << d_ll_block.getName().str() << "\n";

    if (preds_begin() != preds_end()) {
        llvm::errs() << "\t; predecessors:";
        std::for_each(
            preds_begin(), preds_end(),
            [](auto tmp) {
                auto [ pred, edge ] = tmp;

                if (llvm::isa<ClosureBlock>(pred)) {
                    llvm::errs() << " " << llvm::cast<ClosureBlock>(pred)->d_ll_block.getName();
                }
                else if (llvm::isa<NonClosureRegion>(pred)) {
                    llvm::errs() << " (non-closure-region)";
                }
            });
        llvm::errs() << "\n";
    }

    std::for_each(
        insts_begin(), insts_end(),
        [](const auto& inst) {
            inst.dump();
        });

    if (succs_begin() != succs_end()) {
        llvm::errs() << "\t; successors:";
        std::for_each(
            succs_begin(), succs_end(),
            [](auto tmp) {
                auto [ succ, edge ] = tmp;

                if (llvm::isa<ClosureBlock>(succ)) {
                    llvm::errs() << " " << llvm::cast<ClosureBlock>(succ)->d_ll_block.getName();
                }
                else if (llvm::isa<NonClosureRegion>(succ)) {
                    llvm::errs() << " (non-closure-region)";
                }
            });
        llvm::errs() << "\n";
    }

    llvm::errs() << "\n";
}

//
NonClosureRegion::NonClosureRegion(BlockListType&& ll_blocks, ClosureFunction *function)
: Block(Value::ValueKind::NonClosureRegion, function)
, d_ll_blocks(ll_blocks) {
}

const llvm::Value *
NonClosureRegion::getLLValue() const {
    return nullptr;
}

void
NonClosureRegion::dump() const {
    llvm::errs() << "NonClosureRegion:\n";

    if (preds_begin() != preds_end()) {
        llvm::errs() << "\t; predecessors:";
        std::for_each(
            preds_begin(), preds_end(),
            [](auto tmp) {
                auto [ pred, edge ] = tmp;

                if (llvm::isa<ClosureBlock>(pred)) {
                    llvm::errs() << " " << llvm::cast<ClosureBlock>(pred)->d_ll_block.getName();
                }
                else if (llvm::isa<NonClosureRegion>(pred)) {
                    llvm::errs() << " (non-closure-region)";
                }
            });
        llvm::errs() << "\n";
    }

    std::for_each(
        d_ll_blocks.begin(), d_ll_blocks.end(),
        [](const auto ll_block) {
            llvm::errs() << ll_block->getName() << "\n";
        });

    if (succs_begin() != succs_end()) {
        llvm::errs() << "\t; successors:";
        std::for_each(
            succs_begin(), succs_end(),
            [](auto tmp) {
                auto [ succ, edge ] = tmp;

                if (llvm::isa<ClosureBlock>(succ)) {
                    llvm::errs() << " " << llvm::cast<ClosureBlock>(succ)->d_ll_block.getName();
                }
                else if (llvm::isa<NonClosureRegion>(succ)) {
                    llvm::errs() << " (non-closure-region)";
                }
            });
        llvm::errs() << "\n";
    }

    llvm::errs() << "\n";
}

} // End namespace llosl
