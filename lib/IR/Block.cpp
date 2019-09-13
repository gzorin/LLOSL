#include <llosl/IR/Block.h>
#include <llosl/IR/ClosureFunction.h>

#include <llvm/IR/BasicBlock.h>

namespace llosl {

Block::Block(const llvm::BasicBlock& ll_block, ClosureFunction *function)
: Value(Value::ValueKind::Block)
, d_ll_block(ll_block) {
    setParent(function);
}

Block::~Block() {
    //assert(!d_function);
}

void
Block::dropAllReferences() {
    std::for_each(
        d_insts.begin(), d_insts.end(),
        [](auto& instruction) -> void {
            instruction.dropAllReferences();
        });
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

const llvm::Value *
Block::getLLValue() const {
    return &d_ll_block;
}

} // End namespace llosl
