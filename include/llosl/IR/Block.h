//-*-C++-*-
#ifndef LLOSL_IR_BLOCK_H
#define LLOSL_IR_BLOCK_H

#include <llosl/IR/Instruction.h>
#include <llosl/IR/Value.h>

#include <llvm/ADT/DenseMap.h>
#include <llvm/ADT/SmallVector.h>
#include <llvm/ADT/ilist.h>
#include <llvm/ADT/ilist_node.h>

#include <list>

namespace llvm {
class BasicBlock;
class TerminatorInst;
} // end namespace llvm

namespace llosl {

class ClosureFunction;

class Block
    : public Value
    , public llvm::ilist_node_with_parent<Block, ClosureFunction> {
public:
    static bool classof(const Value *value) {
        const auto kind = value->getKind();
        return kind >= Value::ValueKind::Block && kind < Value::ValueKind::BlockMax;
    }

    ~Block() override;

    void             setParent(ClosureFunction *);
    ClosureFunction *getParent() const { return d_function; }

    struct Edge {
        llvm::BasicBlock *from = nullptr, *to = nullptr;
    };

    void insertSuccessor(Block *, Edge);
    void eraseSuccessor(Block *);

    //
    using EdgesType = llvm::SmallVector<Edge, 2>;

    using SuccSetType = llvm::DenseMap<Block *, EdgesType>;

    SuccSetType &      getSuccSet() { return d_successors; }
    const SuccSetType &getSuccSet() const { return d_successors; }

    SuccSetType::iterator       succs_begin() { return d_successors.begin(); }
    SuccSetType::iterator       succs_end() { return d_successors.end(); }
    SuccSetType::const_iterator succs_begin() const { return d_successors.begin(); }
    SuccSetType::const_iterator succs_end() const { return d_successors.end(); }

    //
    using PredSetType = llvm::DenseMap<Block *, EdgesType>;

    PredSetType &      getPredSet() { return d_predecessors; }
    const PredSetType &getPredSet() const { return d_predecessors; }

    PredSetType::iterator       preds_begin() { return d_predecessors.begin(); }
    PredSetType::iterator       preds_end() { return d_predecessors.end(); }
    PredSetType::const_iterator preds_begin() const { return d_predecessors.begin(); }
    PredSetType::const_iterator preds_end() const { return d_predecessors.end(); }

protected:
    Block(Value::ValueKind, ClosureFunction * = nullptr);

    ClosureFunction *d_function = nullptr;

    PredSetType d_predecessors;
    SuccSetType d_successors;
};

class ClosureBlock : public Block {
public:
    static bool classof(const Value *value) {
        return value->getKind() == Value::ValueKind::ClosureBlock;
    }

    ClosureBlock(llvm::BasicBlock &, ClosureFunction * = nullptr);

    void dropAllReferences();

    using InstListType = llvm::iplist<Instruction>;
    static InstListType ClosureBlock::*getSublistAccess(Instruction *) {
        return &ClosureBlock::d_insts;
    }

    InstListType &      getInstList() { return d_insts; }
    const InstListType &getInstList() const { return d_insts; }

    InstListType::iterator       insts_begin() { return d_insts.begin(); }
    InstListType::iterator       insts_end() { return d_insts.end(); }
    InstListType::const_iterator insts_begin() const { return d_insts.begin(); }
    InstListType::const_iterator insts_end() const { return d_insts.end(); }

    //
    llvm::BasicBlock * getLLBlock() { return &d_ll_block; }
    const llvm::Value *getLLValue() const override;

    void dump() const override;

private:
    llvm::BasicBlock &d_ll_block;

    InstListType d_insts;

    friend class NonClosureRegion;
};

class NonClosureRegion : public Block {
public:
    static bool classof(const Value *value) {
        return value->getKind() == Value::ValueKind::NonClosureRegion;
    }

    using BlockListType = std::list<llvm::BasicBlock *>;

    NonClosureRegion(BlockListType &&, ClosureFunction * = nullptr);

    BlockListType &      getBlockList() { return d_ll_blocks; }
    const BlockListType &getBlockList() const { return d_ll_blocks; }

    BlockListType::iterator       blocks_begin() { return d_ll_blocks.begin(); }
    BlockListType::iterator       blocks_end() { return d_ll_blocks.end(); }
    BlockListType::const_iterator blocks_begin() const { return d_ll_blocks.begin(); }
    BlockListType::const_iterator blocks_end() const { return d_ll_blocks.end(); }

    const llvm::Value *getLLValue() const override;

    void dump() const override;

private:
    BlockListType d_ll_blocks;
};

} // End namespace llosl

#endif