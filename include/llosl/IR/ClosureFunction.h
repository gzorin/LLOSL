//-*-C++-*-
#ifndef LLOSL_IR_CLOSUREFUNCTION_H
#define LLOSL_IR_CLOSUREFUNCTION_H

namespace llvm {
class Function;
} // End namespace llvm

namespace llosl {

class ClosureFunction {
public:

    llvm::Function *getLLFunction() const { return d_ll_function; }

private:

    ClosureFunction(llvm::Function &);

    llvm::Function *d_ll_function;
};

} // End namespace llosl

#endif
