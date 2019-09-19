#include <llosl/IR/Block.h>
#include <llosl/IR/Instruction.h>

#include <llvm/IR/Constants.h>
#include <llvm/IR/Instructions.h>
#include <llvm/Support/raw_os_ostream.h>

#include <iostream>

namespace llosl {

Instruction::Instruction(Value::ValueKind kind, unsigned operand_count, Block *block)
: User(kind, operand_count) {
    setParent(block);
}

void
Instruction::setParent(Block *block) {
    if (d_block == block) {
        return;
    }

    if (d_block) {
        d_block->getInstList().remove(*this);
    }

    d_block = block;

    if (d_block) {
        d_block->getInstList().push_back(this);
    }
}

// Alloca
Alloca::Alloca(const llvm::AllocaInst& ll_instruction, unsigned location, Block *block)
: Instruction(Value::ValueKind::Alloca, 0, block)
, d_ll_instruction(ll_instruction)
, d_location(location) {
}

const llvm::Value *
Alloca::getLLValue() const {
    return &d_ll_instruction;
}

void
Alloca::dump() const {
    llvm::errs() << "Alloca " << d_location << '\n';
}

// Load
Load::Load(const llvm::LoadInst& ll_instruction, unsigned location, Block *block)
: Instruction(Value::ValueKind::Load, 1, block)
, d_ll_instruction(ll_instruction)
, d_location(location) {
}

const llvm::Value *
Load::getLLValue() const {
    return &d_ll_instruction;
}

void
Load::dump() const {
    llvm::errs() << "Load " << d_location << '\n';
}

// Store
Store::Store(const llvm::StoreInst& ll_instruction, Value& rhs, unsigned location, Block *block)
: Instruction(Value::ValueKind::Store, 1, block)
, d_ll_instruction(ll_instruction)
, d_rhs(rhs)
, d_location(location) {
    setOperandValue(0, &d_rhs);
}

const llvm::Value *
Store::getLLValue() const {
    return &d_ll_instruction;
}

void
Store::dump() const {
    llvm::errs() << "Store " << d_location << " ";
    d_rhs.getLLValue()->printAsOperand(llvm::errs());
    llvm::errs() << '\n';
}

// AllocateComponent
AllocateComponent::AllocateComponent(const llvm::CallInst& ll_instruction, Block *block)
: Instruction(Value::ValueKind::AllocateComponent, 0, block)
, d_ll_instruction(ll_instruction) {
}

const llvm::Value *
AllocateComponent::getLLValue() const {
    return &d_ll_instruction;
}

unsigned
AllocateComponent::getClosureID() const {
    return llvm::dyn_cast<llvm::ConstantInt>(d_ll_instruction.getArgOperand(1))->getSExtValue();
}

unsigned
AllocateComponent::getClosureSize() const {
    return llvm::dyn_cast<llvm::ConstantInt>(d_ll_instruction.getArgOperand(2))->getSExtValue();
}

// AllocateWeightedComponent
AllocateWeightedComponent::AllocateWeightedComponent(const llvm::CallInst& ll_instruction, Block *block)
: Instruction(Value::ValueKind::AllocateWeightedComponent, 0, block)
, d_ll_instruction(ll_instruction) {
}

const llvm::Value *
AllocateWeightedComponent::getLLValue() const {
    return &d_ll_instruction;
}

unsigned
AllocateWeightedComponent::getClosureID() const {
    return llvm::dyn_cast<llvm::ConstantInt>(d_ll_instruction.getArgOperand(1))->getSExtValue();
}

unsigned
AllocateWeightedComponent::getClosureSize() const {
    return llvm::dyn_cast<llvm::ConstantInt>(d_ll_instruction.getArgOperand(2))->getSExtValue();
}

// AddClosureClosure
AddClosureClosure::AddClosureClosure(const llvm::CallInst& ll_instruction, Value& lhs, Value& rhs, Block *block)
: Instruction(Value::ValueKind::AddClosureClosure, 2, block)
, d_ll_instruction(ll_instruction)
, d_lhs(lhs)
, d_rhs(rhs) {
    setOperandValue(0, &d_lhs);
    setOperandValue(1, &d_rhs);
}

const llvm::Value *
AddClosureClosure::getLLValue() const {
    return &d_ll_instruction;
}

// MulClosureColor
MulClosureColor::MulClosureColor(const llvm::CallInst& ll_instruction, Value& lhs, Block *block)
: Instruction(Value::ValueKind::MulClosureColor, 1, block)
, d_ll_instruction(ll_instruction)
, d_lhs(lhs) {
    setOperandValue(0, &d_lhs);
}

const llvm::Value *
MulClosureColor::getLLValue() const {
    return &d_ll_instruction;
}

// MulClosureFloat
MulClosureFloat::MulClosureFloat(const llvm::CallInst& ll_instruction, Value& lhs, Block *block)
: Instruction(Value::ValueKind::MulClosureFloat, 1, block)
, d_ll_instruction(ll_instruction)
, d_lhs(lhs) {
    setOperandValue(0, &d_lhs);
}

const llvm::Value *
MulClosureFloat::getLLValue() const {
    return &d_ll_instruction;
}

// Cast
Cast::Cast(const llvm::CastInst& ll_instruction, Value& rhs, Block *block)
: Instruction(Value::ValueKind::Cast, 1, block)
, d_ll_instruction(ll_instruction)
, d_rhs(rhs) {
    setOperandValue(0, &d_rhs);
}

const llvm::Value *
Cast::getLLValue() const {
    return &d_ll_instruction;
}

void
Cast::dump() const {
    llvm::errs() << "Cast ";
    d_rhs.getLLValue()->printAsOperand(llvm::errs());
    llvm::errs() << '\n';
}

// PHI
PHI::PHI(const llvm::PHINode& ll_instruction, llvm::ArrayRef<Value *> operands, Block *block)
: Instruction(Value::ValueKind::PHI, operands.size(), block)
, d_ll_instruction(ll_instruction) {
    for (size_t i = 0, n = operands.size(); i < n; ++i) {
        setOperandValue(i, operands[i]);
    }
}

const llvm::Value *
PHI::getLLValue() const {
    return &d_ll_instruction;
}

// Return
Return::Return(const llvm::ReturnInst& ll_instruction, Block *block)
: Instruction(Value::ValueKind::Return, 0, block)
, d_ll_instruction(ll_instruction) {
}

const llvm::Value *
Return::getLLValue() const {
    return &d_ll_instruction;
}

void
Return::dump() const {
    llvm::errs() << "Return\n";
}

} // End namespace llosl
