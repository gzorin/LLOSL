//-*-C++-*-
#ifndef LLOSL_IR_INSTRUCTION_H
#define LLOSL_IR_INSTRUCTION_H

#include <llosl/IR/User.h>

#include <llvm/ADT/ArrayRef.h>
#include <llvm/ADT/ilist_node.h>

namespace llvm {
class AllocaInst;
class CallInst;
class CastInst;
class LoadInst;
class PHINode;
class ReturnInst;
class StoreInst;
} // End namespace llvm

namespace llosl {

class Block;

class Instruction : public User
                  , public llvm::ilist_node_with_parent<Instruction, Block> {
public:

    static bool classof(const Value* value) {
        const auto kind = value->getKind();
        return kind >= Value::ValueKind::Instruction && kind < Value::ValueKind::InstructionMax;
    }

    void setParent(Block *);
    Block *getParent() const { return d_block; }

protected:

    Instruction(Value::ValueKind, unsigned, Block * = nullptr);

private:

    Block *d_block = nullptr;
};

class Alloca : public Instruction {
public:

    static bool classof(const Value* value) {
        return value->getKind() == Value::ValueKind::Alloca;
    }

    Alloca(const llvm::AllocaInst&, unsigned, Block * = nullptr);

    unsigned getLocation() const { return d_location; }

    const llvm::Value *getLLValue() const override;
    void dump() const override;

private:

    const llvm::AllocaInst& d_ll_instruction;
    unsigned d_location;
};

class Load : public Instruction {
public:

    static bool classof(const Value* value) {
        return value->getKind() == Value::ValueKind::Load;
    }

    Load(const llvm::LoadInst&, unsigned, Block * = nullptr);

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

    Store(const llvm::StoreInst&, Value&, unsigned, Block * = nullptr);

    unsigned getLocation() const { return d_location; }

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

    AllocateComponent(const llvm::CallInst&, Block * = nullptr);

    unsigned getClosureID() const;

    const llvm::Value *getLLValue() const override;

private:

    const llvm::CallInst& d_ll_instruction;
};

class AllocateWeightedComponent : public Instruction {
public:

    static bool classof(const Value *value) {
        return value->getKind() == Value::ValueKind::AllocateWeightedComponent;
    }

    AllocateWeightedComponent(const llvm::CallInst&, Block * = nullptr);

    unsigned getClosureID() const;

    const llvm::Value *getLLValue() const override;

private:

    const llvm::CallInst& d_ll_instruction;
};

class AddClosureClosure : public Instruction {
public:

    static bool classof(const Value *value) {
        return value->getKind() == Value::ValueKind::AddClosureClosure;
    }

    AddClosureClosure(const llvm::CallInst&, Value&, Value&, Block * = nullptr);

    Value* getLHS() { return &d_lhs; }
    const Value* getLHS() const { return &d_lhs; }

    Value* getRHS() { return &d_rhs; }
    const Value* getRHS() const { return &d_rhs; }

    const llvm::Value *getLLValue() const override;

private:

    const llvm::CallInst& d_ll_instruction;
    Value& d_lhs;
    Value& d_rhs;
};

class MulClosureColor : public Instruction {
public:

    static bool classof(const Value *value) {
        return value->getKind() == Value::ValueKind::MulClosureColor;
    }

    MulClosureColor(const llvm::CallInst&, Value&, Block * = nullptr);

    Value* getLHS() { return &d_lhs; }
    const Value* getLHS() const { return &d_lhs; }

    const llvm::Value *getLLValue() const override;

private:

    const llvm::CallInst& d_ll_instruction;
    Value& d_lhs;
};

class MulClosureFloat : public Instruction {
public:

    static bool classof(const Value *value) {
        return value->getKind() == Value::ValueKind::MulClosureFloat;
    }

    MulClosureFloat(const llvm::CallInst&, Value&, Block * = nullptr);

    Value* getLHS() { return &d_lhs; }
    const Value* getLHS() const { return &d_lhs; }

    const llvm::Value *getLLValue() const override;

private:

    const llvm::CallInst& d_ll_instruction;
    Value& d_lhs;
};

class Cast : public Instruction {
public:

    static bool classof(const Value* value) {
        return value->getKind() == Value::ValueKind::Cast;
    }

    Cast(const llvm::CastInst&, Value&, Block * = nullptr);

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

    PHI(const llvm::PHINode&, llvm::ArrayRef<Value *>, Block * = nullptr);

    const llvm::Value *getLLValue() const override;

private:

    const llvm::PHINode& d_ll_instruction;
};

class Return : public Instruction {
public:

    static bool classof(const Value* value) {
        return value->getKind() == Value::ValueKind::Return;
    }

    Return(const llvm::ReturnInst&, Block * = nullptr);

    const llvm::Value *getLLValue() const override;

private:

    const llvm::ReturnInst& d_ll_instruction;
};

} // End namespace llosl

#endif