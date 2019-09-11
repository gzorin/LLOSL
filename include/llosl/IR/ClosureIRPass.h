//-*-C++-*-
#ifndef LLOSL_IR_CLOSUREIRPASS_H
#define LLOSL_IR_CLOSUREIRPASS_H

#include <llvm/Pass.h>

namespace llosl {

class ClosureFunction;

class ClosureIRPass : public llvm::FunctionPass {
public:
    static char ID;

    ClosureIRPass();

    ClosureFunction &getIR()             { return *d_closure_function; }
    const ClosureFunction &getIR() const { return *d_closure_function; }

    bool runOnFunction(llvm::Function &F) override;

    void getAnalysisUsage(llvm::AnalysisUsage &AU) const override;

private:

    class Context;

    std::unique_ptr<Context> d_context;

    std::unique_ptr<ClosureFunction> d_closure_function;
};

llvm::FunctionPass *createClosureInfoPass();

} // End namespace llosl

#endif
