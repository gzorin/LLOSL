//-*-C++-*-
#ifndef LLOSL_UTIL_H
#define LLOSL_UTIL_H

#include <llvm/ADT/StringRef.h>

#include <OpenImageIO/string_view.h>

namespace LLOSLUtil {

inline OIIO::string_view ConvertStringRef(llvm::StringRef string_ref) {
  return OIIO::string_view(string_ref.data(), string_ref.size());
}

inline llvm::StringRef ConvertStringRef(OIIO::string_view string_view) {
  return llvm::StringRef(string_view.data(), string_view.size());
}

} // End namespace LLOSLUtil

#endif
