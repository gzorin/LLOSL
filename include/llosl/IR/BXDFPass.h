//-*-C++-*-
#ifndef LLOSL_IR_BXDFPASS_H
#define LLOSL_IR_BXDFPASS_H

#include <llvm/Pass.h>

#include <memory>
#include <variant>

namespace llosl {

class BXDFPass;

struct BXDFVoid;
struct BXDFComponent;
struct BXDFWeightedComponent;
struct BXDFAdd;
struct BXDFMulColor;
struct BXDFMulFloat;

using BXDFNode = std::variant<BXDFVoid, BXDFComponent, BXDFWeightedComponent, BXDFAdd, BXDFMulColor, BXDFMulFloat>;
using BXDFNodeRef = std::shared_ptr<BXDFNode>;

struct BXDFVoid {
};

struct BXDFComponent {
    BXDFComponent(unsigned id, unsigned address)
    : id(id)
    , address(address) {
    }

    unsigned id;
    unsigned address;
};

struct BXDFWeightedComponent {
    BXDFWeightedComponent(unsigned id, unsigned address, unsigned weight_address)
    : id(id)
    , address(address)
    , weight_address(weight_address) {
    }

    unsigned id;
    unsigned address;
    unsigned weight_address;
};

struct BXDFAdd {
    BXDFAdd(BXDFNodeRef lhs, BXDFNodeRef rhs)
    : lhs(lhs)
    , rhs(rhs) {
    }

    BXDFNodeRef lhs, rhs;
};

struct BXDFMulColor {
    BXDFMulColor(BXDFNodeRef lhs, unsigned rhs_address)
    : lhs(lhs)
    , rhs_address(rhs_address) {
    }

    BXDFNodeRef lhs;
    unsigned rhs_address;
};

struct BXDFMulFloat {
    BXDFMulFloat(BXDFNodeRef lhs, unsigned rhs_address)
    : lhs(lhs)
    , rhs_address(rhs_address) {
    }

    BXDFNodeRef lhs;
    unsigned rhs_address;
};

class BXDFInfo {
public:

    struct BXDF {
        BXDFNodeRef ast;
        unsigned heap_size = 0;
    };

    const BXDF& getBXDFForPath(unsigned path_id) const { return d_bxdfs[path_id]; }

    unsigned getMaxHeapSize() const { return d_max_heap_size; }

private:

    BXDFInfo(unsigned);

    void addBXDFForPath(unsigned, BXDFNodeRef, unsigned);

    std::vector<BXDF> d_bxdfs;
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
