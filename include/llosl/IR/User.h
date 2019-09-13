//-*-C++-*-
#ifndef LLOSL_IR_USER_H
#define LLOSL_IR_USER_H

#include <llosl/IR/Use.h>
#include <llosl/IR/Value.h>

#include <memory>

namespace llosl {

class User : public Value {
public:

    void dropAllReferences();

    Use *getOperand(unsigned operand_index) {
        assert(operand_index < d_operand_count);
        return &d_operands[operand_index];
    }

    const Use *getOperand(unsigned operand_index) const {
        assert(operand_index < d_operand_count);
        return &d_operands[operand_index];
    }

    Value *getOperandValue(unsigned operand_index) {
        return getOperand(operand_index)->get();
    }

    const Value *getOperandValue(unsigned operand_index) const {
        return getOperand(operand_index)->get();
    }

    Use *operand_begin() { return &d_operands[0]; }
    const Use *operand_begin() const { return &d_operands[0]; };

    Use *operand_end() { return &d_operands[d_operand_count - 1]; }
    const Use *operand_end() const { return &d_operands[d_operand_count - 1]; };

protected:

    User(ValueKind, unsigned);

    void setOperandValue(unsigned, Value *);

    std::unique_ptr<Use[]> d_operands;
    unsigned d_operand_count = 0;
};

} // End namespace llosl

#endif
