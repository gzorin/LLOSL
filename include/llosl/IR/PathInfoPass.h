//-*-C++-*-
#ifndef LLOSL_IR_PATHINFOPASS_H
#define LLOSL_IR_PATHINFOPASS_H

#include <llvm/ADT/DenseMap.h>
#include <llvm/ADT/Optional.h>
#include <llvm/Pass.h>

namespace llosl {

class Block;
class ClosureFunction;

class PathInfoPass;

class PathInfo {
public:

    using EdgeIDMap = llvm::DenseMap<const Block *, unsigned>;

    struct BlockInfo {
        unsigned path_count = 0;
        EdgeIDMap edge_id;
    };

    llvm::Optional<const BlockInfo *> getInfoForBlock(const Block *) const;
    llvm::Optional<unsigned> getPathCountForBlock(const Block *) const;
    llvm::Optional<const EdgeIDMap *> getEdgeIDsForBlock(const Block *) const;
    llvm::Optional<unsigned> getEdgeIDForBlocks(const Block *, const Block *) const;

private:

    void insertLeaf(const Block *);
    void insertEdge(const Block *, const Block *);

    llvm::DenseMap<const Block *, BlockInfo> d_block_info;

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
