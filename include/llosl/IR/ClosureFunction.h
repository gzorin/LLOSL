//-*-C++-*-
#ifndef LLOSL_IR_CLOSUREFUNCTION_H
#define LLOSL_IR_CLOSUREFUNCTION_H

#include <llosl/IR/Block.h>
#include <llosl/IR/Value.h>

#include <llvm/ADT/ilist.h>

namespace llvm {
class Function;
} // End namespace llvm

namespace llosl {

class ClosureFunction : public Value {
public:

    static bool classof(const Value* value) {
        return value->getKind() == Value::ValueKind::Function;
    }

    ClosureFunction(unsigned);
    ~ClosureFunction() override;

    unsigned getClosureStorageCount() const { return d_closure_storage_count; }

    using BlockListType = llvm::ilist<Block>;
    static BlockListType ClosureFunction::*getSublistAccess(Block *) {
        return &ClosureFunction::d_blocks;
    }

    BlockListType&       getBlockList()       { return d_blocks; }
    const BlockListType& getBlockList() const { return d_blocks; }

    BlockListType::iterator       blocks_begin()       { return d_blocks.begin(); }
    BlockListType::iterator       blocks_end()         { return d_blocks.end();   }
    BlockListType::const_iterator blocks_begin() const { return d_blocks.begin(); }
    BlockListType::const_iterator blocks_end()   const { return d_blocks.end();   }

    //
    const llvm::Value *getLLValue() const override;

private:

    unsigned d_closure_storage_count;

    BlockListType d_blocks;
};

} // End namespace llosl

#endif
