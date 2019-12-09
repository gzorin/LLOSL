//-*-C++-*-
#ifndef LLOSL_IR_VALUE_H
#define LLOSL_IR_VALUE_H

#include <llosl/IR/Use.h>

#include <llvm/ADT/simple_ilist.h>
#include <llvm/Support/Casting.h>

namespace llvm {
class Value;
} // End namespace llvm

namespace llosl {

class Value {
public:

    enum class ValueKind {
        Instruction,
        Reference,
        Null,
        Load,
        Store,
        AllocateComponent,
        AllocateWeightedComponent,
        AddClosureClosure,
        MulClosureColor,
        MulClosureFloat,
        Cast,
        PHI,
        Return,
        InstructionMax,
        Output,
        Function,
        Block
    };

    Value(Value&) = delete;
    Value(Value&&) = delete;
    virtual ~Value();

    ValueKind getKind() const { return d_kind; }

    virtual const llvm::Value *getLLValue() const = 0;

    virtual void dump() const;

protected:

    Value(ValueKind);

private:

    ValueKind d_kind;
    llvm::simple_ilist<Use> d_uses;

    friend class Use;
};

class Output : public Value {
public:

    Output(const llvm::Value&);

    static bool classof(const Value* value) {
        return value->getKind() == Value::ValueKind::Output;
    }

    const llvm::Value *getLLValue() const override;

protected:

    const llvm::Value& d_ll_value;
};

} // End namespace llosl

#endif
