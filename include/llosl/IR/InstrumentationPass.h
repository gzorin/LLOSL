//-*-C++-*-
#ifndef LLOSL_IR_INSTRUMENTATIONPASS_H
#define LLOSL_IR_INSTRUMENTATIONPASS_H

#include <llvm/Pass.h>

namespace llosl {

class ClosureFunction;

class InstrumentationPass : public llvm::FunctionPass {
public:
    static char ID;

    InstrumentationPass();

    bool runOnFunction(llvm::Function &F) override;

    void getAnalysisUsage(llvm::AnalysisUsage &AU) const override;

private:

};

llvm::FunctionPass *createInstrumentationPass();

} // End namespace llosl

#endif
