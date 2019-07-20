#include "LLOSLContextImpl.h"
#include "Util.h"

#include <llosl/Builder.h>

#include <OSL/oslexec.h>

#include <utility>

using namespace LLOSLUtil;

namespace llosl {

Builder::Builder(LLOSLContextImpl& context)
    : d_context(&context) {
    d_context->resetBuilder(this);
}

Builder::Builder(Builder&& rhs)
    : d_context(rhs.reset()) {
    d_context->resetBuilder(this);
}

Builder::~Builder() {
    reset();
}

LLOSLContextImpl *
Builder::reset() {
  auto context = std::exchange(d_context, nullptr);

  if (context) {
      context->resetBuilder(nullptr);
  }

  return context;
}

void
Builder::BeginShaderGroup(llvm::StringRef name, llvm::StringRef usage) {
  if (!d_context) {
      return;
  }

  d_context->getShadingSystem().ShaderGroupBegin(ConvertStringRef(name), ConvertStringRef(usage));
}

void
Builder::EndShaderGroup() {
  if (!d_context) {
      return;
  }

  d_context->getShadingSystem().ShaderGroupEnd();
}

} // End namespace llosl
