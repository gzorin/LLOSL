#include <llosl/IR/Block.h>
#include <llosl/IR/ClosureFunction.h>

#include <llvm/IR/Function.h>

namespace llosl {

ClosureFunction::ClosureFunction(llvm::Function &ll_function, unsigned closure_storage_count)
: Value(Value::ValueKind::Function)
, d_ll_function(&ll_function)
, d_closure_storage_count(closure_storage_count) {
}

ClosureFunction::~ClosureFunction() {
    std::for_each(
        d_blocks.begin(), d_blocks.end(),
        [](auto& block) -> void {
            block.dropAllReferences();
        });

    while (!d_blocks.empty()) {
        d_blocks.begin()->setParent(nullptr);
    }
}

void
ClosureFunction::updateLLFunction(const llvm::Function &ll_function) const {
    d_ll_function = &ll_function;
}

const llvm::Value *
ClosureFunction::getLLValue() const {
    return d_ll_function;
}

} // End namespace llosl