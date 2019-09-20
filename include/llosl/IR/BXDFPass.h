//-*-C++-*-
#ifndef LLOSL_IR_BXDFPASS_H
#define LLOSL_IR_BXDFPASS_H

#include <llosl/IR/BXDFAST.h>

#include <llvm/Pass.h>

#include <memory>

namespace llosl {

class BXDFPass;

class BXDFInfo {
public:

    unsigned getPathCount() const { return d_bxdfs.size(); }

    const BXDFAST& getBXDFForPath(unsigned path_id) const { return d_bxdfs[path_id]; }

    unsigned getMaxHeapSize() const { return d_max_heap_size; }

private:

    BXDFInfo(unsigned);

    void addBXDFForPath(unsigned, BXDFAST::NodeRef, unsigned);

    std::vector<BXDFAST> d_bxdfs;
    unsigned d_max_heap_size = 0;

    friend class BXDFPass;
};

class BXDFPass : public llvm::FunctionPass {
public:
    static char ID;

    BXDFPass();

    std::shared_ptr<const BXDFInfo> getBXDFInfo() const { return d_bxdf_info; }

    bool runOnFunction(llvm::Function &F) override;

    void getAnalysisUsage(llvm::AnalysisUsage &AU) const override;

private:

    std::shared_ptr<const BXDFInfo> d_bxdf_info;
};

llvm::FunctionPass *createBXDF();

} // End namespace llosl

#endif
