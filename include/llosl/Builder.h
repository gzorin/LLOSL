//-*-C++-*-
#ifndef LLOSL_BUILDER_H
#define LLOSL_BUILDER_H

#include <llvm/ADT/StringRef.h>
#include <llvm/Support/Error.h>

namespace llosl {

class LLOSLContextImpl;

class Builder {
public:

  class Error : public llvm::ErrorInfo<Error> {
  public:

      static char ID;

      enum class Code {
	  InvalidContext = 1,
	  InvalidError   = 2
      };

      Error(Code);

      void log(llvm::raw_ostream&) const override;

      std::error_code convertToErrorCode() const override;

  private:

      Code d_code;
  };

  class OSLError : public llvm::ErrorInfo<OSLError, Error> {
  };

  Builder() = delete;
  Builder(const Builder&) = delete;
  Builder(Builder&&);
  ~Builder();

  Builder& operator=(const Builder&) = delete;
  Builder& operator=(Builder&&);

  llvm::Error BeginShaderGroup(llvm::StringRef, llvm::StringRef);
  llvm::Error EndShaderGroup();

  llvm::Error AddNode(llvm::StringRef, llvm::StringRef, llvm::StringRef);

private:

  friend class LLOSLContextImpl;

  enum class State {
      kValid, kInvalidContext, kInvalidError
  };

  Builder(LLOSLContextImpl& context);

  State d_state = State::kValid;
  LLOSLContextImpl *d_context;
};

} // End namespace llosl

#endif
