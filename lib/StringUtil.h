//-*-C++--
#ifndef LLOSL_STRINGUTIL_H
#define LLOSL_STRINGUTIL_H

#include <llvm/ADT/StringRef.h>

#include <OSL/oslconfig.h>

namespace llosl {

inline
llvm::StringRef
makeLLVMStringRef(OSL::ustring string) {
    return llvm::StringRef(string.data(), string.length());
}

inline
OSL::ustring
makeOSLUString(llvm::StringRef string_ref) {
    return OSL::ustring(string_ref.data(), string_ref.size());
}

}

#endif