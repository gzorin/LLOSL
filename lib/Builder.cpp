#include <llosl/Builder.h>

#include "LLOSLContextImpl.h"

#include <utility>

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

} // End namespace llosl
