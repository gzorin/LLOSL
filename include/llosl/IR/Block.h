//-*-C++-*-
#ifndef LLOSL_IR_BLOCK_H
#define LLOSL_IR_BLOCK_H

#include <llosl/IR/Instruction.h>
#include <llosl/IR/Value.h>

#include <llvm/ADT/ilist.h>
#include <llvm/ADT/ilist_node.h>

namespace llvm {
class BasicBlock;
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

    //
    const llvm::Value *getLLValue() const override;

private:

    const llvm::BasicBlock& d_ll_block;

    ClosureFunction *d_function = nullptr;

    InstListType d_insts;
};

} // End namespace llosl

#endif