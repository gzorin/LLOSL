//-*-C++-*-
#ifndef LLOSL_BUILDERIMPL_H
#define LLOSL_BUILDERIMPL_H

#include <llosl/Builder.h>

#include <OSL/oslexec.h>

namespace llosl {

class LLOSLContextImpl;

class BuilderImpl {
public:

    using Error = Builder::Error;

    BuilderImpl(LLOSLContextImpl&);
    ~BuilderImpl();

    BuilderImpl() = delete;
    BuilderImpl(const BuilderImpl&) = delete;
    BuilderImpl(BuilderImpl&&) = delete;

    const LLOSLContextImpl& context() const { return *d_context; }
    LLOSLContextImpl&       context()       { return *d_context; }

    OSL::ShaderGroupRef     shader_group()  { return d_shader_group; }

    llvm::Error BeginShaderGroup(llvm::StringRef, llvm::StringRef);
    llvm::Error EndShaderGroup();

    llvm::Error AddNode(llvm::StringRef, llvm::StringRef, llvm::StringRef);

    llvm::Expected<Shader *> Finalize();

private:

    enum class State {
	kValid, kInvalidContext, kInvalidError
    };

    State d_state = State::kValid;
    LLOSLContextImpl *d_context;

    OSL::ShaderGroupRef d_shader_group;
};

} // End namespace llosl

#endif
