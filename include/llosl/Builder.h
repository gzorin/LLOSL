//-*-C++-*-
#ifndef LLOSL_BUILDER_H
#define LLOSL_BUILDER_H

#include <llvm/ADT/StringRef.h>
#include <llvm/Support/Error.h>

#include <memory>

namespace llosl {

class BuilderImpl;
class LLOSLContextImpl;
class Shader;

class Builder {
public:
    class Error : public llvm::ErrorInfo<Error> {
    public:
        static char ID;

        enum class Code { InvalidContext = 1, InvalidError = 2 };

        Error(Code);

        void log(llvm::raw_ostream &) const override;

        std::error_code convertToErrorCode() const override;

    private:
        Code d_code;
    };

    class OSLError : public llvm::ErrorInfo<OSLError, Error> {};

    Builder()                = delete;
    Builder(const Builder &) = delete;
    Builder(Builder &&);
    ~Builder();

    Builder &operator=(const Builder &) = delete;
    Builder &operator                   =(Builder &&);

    llvm::Error BeginShaderGroup(llvm::StringRef, llvm::StringRef);
    llvm::Error EndShaderGroup();

    llvm::Error AddNode(llvm::StringRef, llvm::StringRef, llvm::StringRef);

    llvm::Expected<Shader *> Finalize();

private:
    friend class LLOSLContextImpl;

    Builder(LLOSLContextImpl &context);

    std::unique_ptr<BuilderImpl> d_impl;
};

} // End namespace llosl

#endif
