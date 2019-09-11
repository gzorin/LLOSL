#include <llosl/IR/ClosureFunction.h>

namespace llosl {

ClosureFunction::ClosureFunction(llvm::Function &ll_function)
: d_ll_function(&ll_function) {
}

} // End namespace llosl