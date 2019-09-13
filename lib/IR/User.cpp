#include <llosl/IR/User.h>

namespace llosl {

User::User(Value::ValueKind kind, unsigned operand_count)
: Value(kind)
, d_operand_count(operand_count) {
    if (d_operand_count > 0) {
        d_operands.reset(new Use[d_operand_count]);
    }
}

void
User::dropAllReferences() {
    for (unsigned i = 0; i < d_operand_count; ++i) {
        setOperandValue(i, nullptr);
    }
}

void
User::setOperandValue(unsigned operand_index, Value *value) {
    assert(operand_index < d_operand_count);
    getOperand(operand_index)->set(value);
}

} // End namespace llosl
