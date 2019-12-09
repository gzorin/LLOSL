//-*-C++-*-
#ifndef LLOSL_IR_INSTRUMENTATIONPASS_H
#define LLOSL_IR_INSTRUMENTATIONPASS_H

#include <llvm/ADT/StringRef.h>

namespace llvm {
class Function;
} // End namespace llvm

namespace llosl {

class PathInfo;

llvm::Function *InstrumentFunctionForPathId(llvm::Function&, const PathInfo&, llvm::StringRef = "");

} // End namespace llosl

#endif
