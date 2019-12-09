//-*-C++-*-
#ifndef LLOSL_IR_BLOCK_H
#define LLOSL_IR_BLOCK_H

#include <llosl/IR/Instruction.h>
#include <llosl/IR/Value.h>

#include <llvm/ADT/DenseSet.h>
#include <llvm/ADT/ilist.h>
#include <llvm/ADT/ilist_node.h>

namespace llvm {
class BasicBlock;
class TerminatorInst;
} // end namespace llvm

namespace llosl {

class ClosureFunction;

class Block : public Value
            , public llvm::ilist_node_with_parent<Block, ClosureFunction> {
public:

    static bool classof(const Value* value) {
        return value->getKind() == Value::ValueKind::Block;
    }

    Block(const llvm::BasicBlock&, ClosureFunction * = nullptr);
    ~Block() override;

    void dropAllReferences();

    void setParent(ClosureFunction *);
    ClosureFunction *getParent() const { return d_function; }

    using InstListType = llvm::iplist<Instruction>;
    static InstListType Block::*getSublistAccess(Instruction *) {
        return &Block::d_insts;
    }

    InstListType&       getInstList()       { return d_insts; }
    const InstListType& getInstList() const { return d_insts; }

    InstListType::iterator       insts_begin()       { return d_insts.begin(); }
    InstListType::iterator       insts_end()         { return d_insts.end();   }
    InstListType::const_iterator insts_begin() const { return d_insts.begin(); }
    InstListType::const_iterator insts_end()   const { return d_insts.end();   }

    void insertSuccessor(Block *, const llvm::TerminatorInst *, unsigned);
    void eraseSuccessor(Block *);

    //
    using TerminatorSetType = llvm::DenseSet<std::pair<const llvm::TerminatorInst *, unsigned> >;

    using SuccSetType = llvm::DenseMap<Block *, TerminatorSetType>;

    SuccSetType& getSuccSet()             { return d_successors; }
    const SuccSetType& getSuccSet() const { return d_successors; }

    SuccSetType::iterator       succs_begin()       { return d_successors.begin(); }
    SuccSetType::iterator       succs_end()         { return d_successors.end();   }
    SuccSetType::const_iterator succs_begin() const { return d_successors.begin(); }
    SuccSetType::const_iterator succs_end()   const { return d_successors.end();   }

    //
    using PredSetType = llvm::DenseMap<Block *, TerminatorSetType>;

    PredSetType& getPredSet()             { return d_predecessors; }
    const PredSetType& getPredSet() const { return d_predecessors; }

    PredSetType::iterator       preds_begin()       { return d_predecessors.begin(); }
    PredSetType::iterator       preds_end()         { return d_predecessors.end();   }
    PredSetType::const_iterator preds_begin() const { return d_predecessors.begin(); }
    PredSetType::const_iterator preds_end()   const { return d_predecessors.end();   }

    //
    const llvm::Value *getLLValue() const override;

    void dump() const override;

private:

    const llvm::BasicBlock& d_ll_block;

    ClosureFunction *d_function = nullptr;

    InstListType d_insts;

    PredSetType d_predecessors;
    SuccSetType d_successors;
};

} // End namespace llosl

#endif