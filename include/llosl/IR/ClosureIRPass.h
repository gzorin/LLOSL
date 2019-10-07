//-*-C++-*-
#ifndef LLOSL_IR_CLOSUREIRPASS_H
#define LLOSL_IR_CLOSUREIRPASS_H

#include <llvm/Pass.h>

#include <memory>

namespace llosl {

class ClosureFunction;
class ShaderGroup;

class ClosureIRPass : public llvm::FunctionPass {
public:
    static char ID;

    ClosureIRPass();
    ClosureIRPass(ShaderGroup&);

    std::shared_ptr<const ClosureFunction> getIR() const { return d_closure_function; }

    bool runOnFunction(llvm::Function &F) override;

    void getAnalysisUsage(llvm::AnalysisUsage &AU) const override;

private:

    class Context;

    ShaderGroup* d_shader_group = nullptr;

    std::shared_ptr<const ClosureFunction> d_closure_function;
};

} // End namespace llosl

#endif
