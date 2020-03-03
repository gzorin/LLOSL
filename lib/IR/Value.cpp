#include <llosl/IR/Value.h>

#include <llvm/IR/Value.h>

namespace llosl {

Value::Value(ValueKind kind)
    : d_kind(kind) {}

Value::~Value() {
    assert(d_uses.empty());
}

void
Value::dump() const {
    getLLValue()->dump();
}

//
Output::Output(const llvm::Value &ll_value)
    : Value(ValueKind::Output)
    , d_ll_value(ll_value) {}

const llvm::Value *
Output::getLLValue() const {
    return &d_ll_value;
}

} // End namespace llosl
