//-*-C++-*-
#ifndef LLOSL_IR_CLOSUREIRPASS_H
#define LLOSL_IR_CLOSUREIRPASS_H

#include <llvm/Pass.h>

#include <memory>

namespace llosl {

class ClosureFunction;

class ClosureIRPass : public llvm::FunctionPass {
public:
    static char ID;

    ClosureIRPass();

    std::shared_ptr<const ClosureFunction> getIR() const { return d_closure_function; }

    bool runOnFunction(llvm::Function &F) override;

    void getAnalysisUsage(llvm::AnalysisUsage &AU) const override;

private:

    class Context;

    std::shared_ptr<const ClosureFunction> d_closure_function;
};

llvm::FunctionPass *createClosureIRPass();

} // End namespace llosl

#endif
