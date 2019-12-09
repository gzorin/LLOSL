#include <llosl/IR/Block.h>
#include <llosl/IR/ClosureFunction.h>

#include <llvm/IR/Function.h>
#include <llvm/Support/raw_os_ostream.h>

namespace llosl {

ClosureFunction::ClosureFunction(unsigned closure_storage_count,
                                 const std::set<unsigned>& closure_output_locations,
                                 std::optional<unsigned> Ci_location)
: Value(Value::ValueKind::Function)
, d_closure_storage_count(closure_storage_count)
, d_closure_output_locations(closure_output_locations)
, d_Ci_location(Ci_location) {
}

ClosureFunction::~ClosureFunction() {
    std::for_each(
        d_blocks.begin(), d_blocks.end(),
        [](auto& block) -> void {
            if (llvm::isa<ClosureBlock>(&block)) {
                llvm::cast<ClosureBlock>(&block)->dropAllReferences();
            }
        });

    while (!d_blocks.empty()) {
        d_blocks.begin()->setParent(nullptr);
    }
}

const llvm::Value *
ClosureFunction::getLLValue() const {
    return nullptr;
}

void
ClosureFunction::dump() const {
    llvm::errs();

    std::for_each(
        blocks_begin(), blocks_end(),
        [](const auto& block) -> void {
            block.dump();
        });
}

} // End namespace llosl