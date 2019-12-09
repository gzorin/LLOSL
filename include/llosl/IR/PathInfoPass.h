//-*-C++-*-
#ifndef LLOSL_IR_PATHINFOPASS_H
#define LLOSL_IR_PATHINFOPASS_H

#include <llvm/ADT/DenseMap.h>
#include <llvm/ADT/Optional.h>
#include <llvm/Pass.h>

#include <list>

namespace llosl {

class Block;
class ClosureFunction;

class PathInfoPass;

class PathInfo {
public:

    PathInfo(std::shared_ptr<ClosureFunction>);

    std::shared_ptr<ClosureFunction> getIR() const { return d_ir; }

    using EdgeIDMap = llvm::DenseMap<const Block *, unsigned>;

    struct BlockInfo {
        unsigned path_count = 0;
        EdgeIDMap edge_id;
    };

    using BlockInfoMap = llvm::DenseMap<const Block *, BlockInfo>;

    llvm::Optional<const BlockInfo *> getInfoForBlock(const Block *) const;
    llvm::Optional<unsigned> getPathCountForBlock(const Block *) const;
    llvm::Optional<const EdgeIDMap *> getEdgeIDsForBlock(const Block *) const;
    llvm::Optional<unsigned> getEdgeIDForBlocks(const Block *, const Block *) const;

    // The list of blocks in topological order:
    using BlockList = std::list<const Block *>;

    BlockList&       getBlockList()       { return d_block_list; }
    const BlockList& getBlockList() const { return d_block_list; }

    BlockList::iterator       blocks_begin()       { return d_block_list.begin(); }
    BlockList::iterator       blocks_end()         { return d_block_list.end();   }

    BlockList::const_iterator blocks_begin() const { return d_block_list.begin(); }
    BlockList::const_iterator blocks_end()   const { return d_block_list.end();   }

    BlockList::reverse_iterator       blocks_rbegin()       { return d_block_list.rbegin(); }
    BlockList::reverse_iterator       blocks_rend()         { return d_block_list.rend();   }

    BlockList::const_reverse_iterator blocks_rbegin() const { return d_block_list.rbegin(); }
    BlockList::const_reverse_iterator blocks_rend()   const { return d_block_list.rend();   }

    unsigned getPathCount() const { return *getPathCountForBlock(*blocks_begin()); }

private:

    void insertLeaf(const Block *);
    void insertEdge(const Block *, const Block *);

    std::shared_ptr<ClosureFunction> d_ir;
    std::list<const Block *> d_block_list;
    BlockInfoMap d_block_info;

    friend class PathInfoPass;
};

class PathInfoPass : public llvm::FunctionPass {
public:
    static char ID;

    PathInfoPass();

    std::shared_ptr<const PathInfo> getPathInfo() const { return d_path_info; }

    bool runOnFunction(llvm::Function &F) override;

    void getAnalysisUsage(llvm::AnalysisUsage &AU) const override;

private:

    std::shared_ptr<const PathInfo> d_path_info;
};

llvm::FunctionPass *createPathInfo();

} // End namespace llosl

#endif
