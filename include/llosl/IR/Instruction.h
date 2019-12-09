//-*-C++-*-
#ifndef LLOSL_IR_INSTRUCTION_H
#define LLOSL_IR_INSTRUCTION_H

#include <llosl/IR/User.h>

#include <llvm/ADT/ArrayRef.h>
#include <llvm/ADT/ilist_node.h>
#include <llvm/IR/ValueHandle.h>

namespace llvm {
class CallInst;
class CastInst;
class LoadInst;
class PHINode;
class ReturnInst;
class StoreInst;
} // End namespace llvm

namespace llosl {

class ClosureBlock;

class Instruction : public User
                  , public llvm::ilist_node_with_parent<Instruction, ClosureBlock> {
public:

    static bool classof(const Value* value) {
        const auto kind = value->getKind();
        return kind >= Value::ValueKind::Instruction && kind < Value::ValueKind::InstructionMax;
    }

    void setParent(ClosureBlock *);
    ClosureBlock *getParent() const { return d_block; }

protected:

    Instruction(Value::ValueKind, unsigned, ClosureBlock * = nullptr);

private:

    ClosureBlock *d_block = nullptr;
};

class Reference : public Instruction {
public:

    static bool classof(const Value* value) {
        return value->getKind() == Value::ValueKind::Reference;
    }

    Reference(const llvm::Value&, unsigned, ClosureBlock * = nullptr);

    unsigned getLocation() const { return d_location; }

    const llvm::Value *getLLValue() const override;
    void dump() const override;

private:

    const llvm::Value& d_ll_value;
    unsigned d_location;
};

class Null : public Instruction {
public:

    static bool classof(const Value* value) {
        return value->getKind() == Value::ValueKind::Null;
    }

    Null(const llvm::Value&, ClosureBlock * = nullptr);

    const llvm::Value *getLLValue() const override;

private:

    const llvm::Value& d_ll_value;
};

class Load : public Instruction {
public:

    static bool classof(const Value* value) {
        return value->getKind() == Value::ValueKind::Load;
    }

    Load(const llvm::LoadInst&, unsigned, ClosureBlock * = nullptr);

    unsigned getLocation() const { return d_location; }

    const llvm::Value *getLLValue() const override;
    void dump() const override;

private:

    const llvm::LoadInst& d_ll_instruction;
    unsigned d_location;
};

class Store : public Instruction {
public:

    static bool classof(const Value* value) {
        return value->getKind() == Value::ValueKind::Store;
    }

    Store(const llvm::StoreInst&, Value&, unsigned, ClosureBlock * = nullptr);

    unsigned getLocation() const { return d_location; }

    Value *getOperand()             { return &d_rhs; }
    const Value *getOperand() const { return &d_rhs; }

    const llvm::Value *getLLValue() const override;
    void dump() const override;

private:

    const llvm::StoreInst& d_ll_instruction;
    Value& d_rhs;
    unsigned d_location;
};

class AllocateComponent : public Instruction {
public:

    static bool classof(const Value *value) {
        return value->getKind() == Value::ValueKind::AllocateComponent;
    }

    AllocateComponent(const llvm::CallInst&, ClosureBlock * = nullptr);

    unsigned getClosureID() const;
    unsigned getClosureSize() const;

    const llvm::Value *getLLValue() const override;

private:

    const llvm::CallInst& d_ll_instruction;
};

class AllocateWeightedComponent : public Instruction {
public:

    static bool classof(const Value *value) {
        return value->getKind() == Value::ValueKind::AllocateWeightedComponent;
    }

    AllocateWeightedComponent(const llvm::CallInst&, ClosureBlock * = nullptr);

    unsigned getClosureID() const;
    unsigned getClosureSize() const;

    const llvm::Value *getLLValue() const override;

private:

    const llvm::CallInst& d_ll_instruction;
};

class AddClosureClosure : public Instruction {
public:

    static bool classof(const Value *value) {
        return value->getKind() == Value::ValueKind::AddClosureClosure;
    }

    AddClosureClosure(const llvm::CallInst&, unsigned, unsigned, ClosureBlock * = nullptr);

    unsigned getLHS() const { return d_lhs; }

    unsigned getRHS() const { return d_rhs; }

    const llvm::Value *getLLValue() const override;

private:

    const llvm::CallInst& d_ll_instruction;
    unsigned d_lhs;
    unsigned d_rhs;
};

class MulClosureColor : public Instruction {
public:

    static bool classof(const Value *value) {
        return value->getKind() == Value::ValueKind::MulClosureColor;
    }

    MulClosureColor(const llvm::CallInst&, unsigned, ClosureBlock * = nullptr);

    unsigned getLHS() const { return d_lhs; }

    const llvm::Value *getLLValue() const override;

private:

    const llvm::CallInst& d_ll_instruction;
    unsigned d_lhs;
};

class MulClosureFloat : public Instruction {
public:

    static bool classof(const Value *value) {
        return value->getKind() == Value::ValueKind::MulClosureFloat;
    }

    MulClosureFloat(const llvm::CallInst&, unsigned, ClosureBlock * = nullptr);

    unsigned getLHS() const { return d_lhs; }

    const llvm::Value *getLLValue() const override;

private:

    const llvm::CallInst& d_ll_instruction;
    unsigned d_lhs;
};

class Cast : public Instruction {
public:

    static bool classof(const Value* value) {
        return value->getKind() == Value::ValueKind::Cast;
    }

    Cast(const llvm::CastInst&, Value&, ClosureBlock * = nullptr);

    Value *getOperand()             { return &d_rhs; }
    const Value *getOperand() const { return &d_rhs; }

    const llvm::Value *getLLValue() const override;
    void dump() const override;

private:

    const llvm::CastInst& d_ll_instruction;
    Value& d_rhs;
};

class PHI : public Instruction {
public:

    static bool classof(const Value* value) {
        return value->getKind() == Value::ValueKind::PHI;
    }

    PHI(const llvm::PHINode&, llvm::ArrayRef<Value *>, ClosureBlock * = nullptr);

    const llvm::Value *getLLValue() const override;

private:

    const llvm::PHINode& d_ll_instruction;
};

class Return : public Instruction {
public:

    static bool classof(const Value* value) {
        return value->getKind() == Value::ValueKind::Return;
    }

    Return(llvm::ReturnInst&, ClosureBlock * = nullptr);

    const llvm::Value *getLLValue() const override;
    void dump() const override;

private:

    llvm::TrackingVH<llvm::ReturnInst> d_ll_instruction;
};

} // End namespace llosl

#endif