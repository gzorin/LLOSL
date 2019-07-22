#include "LLOSLContextImpl.h"
#include "Util.h"

#include <llosl/Builder.h>

#include <OSL/oslexec.h>

#include <utility>

using namespace LLOSLUtil;

namespace llosl {

namespace {
    
struct ErrorCategory : std::error_category {
    static const ErrorCategory& Get() {
	static ErrorCategory s_error_category;
	return s_error_category;
    }

    const char *name() const noexcept override {
	return "LLOSLBuilder";
    };

    std::string message(int code) const override {
	switch (static_cast<Builder::Error::Code>(code)) {
	case Builder::Error::Code::InvalidContext:
	    return "invalid due to lost context";
	case Builder::Error::Code::InvalidError:
	    return "invalid due to previous error";
	}
    };
};

} // End anonymous namespace

char Builder::Error::ID = 0;

Builder::Error::Error(Code code)
    : d_code(code) {
}

void
Builder::Error::log(llvm::raw_ostream& os) const {
    auto error_code = convertToErrorCode();
    os << error_code.message();
}

std::error_code Builder::Error::convertToErrorCode() const {
    return std::error_code(static_cast<int>(d_code), ErrorCategory::Get());
}

Builder::Builder(LLOSLContextImpl& context)
    : d_context(&context)
    , d_state(State::kValid) {
    d_context->resetBuilder(this);
}

Builder::Builder(Builder&& rhs)
    : d_context(nullptr)
    , d_state(State::kInvalidContext) {
    *this = std::move(rhs);
}

Builder::~Builder() {
    if (d_context) {
	d_context->resetBuilder(nullptr);
    }
}

Builder&
Builder::operator =(Builder&& rhs) {
    if (d_context) {
	d_context->resetBuilder(nullptr);
    }

    std::swap(d_context, rhs.d_context);
    std::swap(d_state, rhs.d_state);

    if (d_context) {
	d_context->resetBuilder(this);
    }

    return *this;
}

llvm::Error
Builder::BeginShaderGroup(llvm::StringRef name, llvm::StringRef usage) {
  switch(d_state) {
  case State::kInvalidContext:
      return llvm::make_error<Error>(Error::Code::InvalidContext);
  case State::kInvalidError:
      return llvm::make_error<Error>(Error::Code::InvalidError);
  default:
      break;
  }

  auto osl_error_scope = d_context->enterOSLErrorScope();

  d_context->getShadingSystem().ShaderGroupBegin(ConvertStringRef(name), ConvertStringRef(usage));

  return osl_error_scope.takeError();
}

llvm::Error
Builder::EndShaderGroup() {
  switch(d_state) {
  case State::kInvalidContext:
      return llvm::make_error<Error>(Error::Code::InvalidContext);
  case State::kInvalidError:
      return llvm::make_error<Error>(Error::Code::InvalidError);
  default:
      break;
  }

  auto osl_error_scope = d_context->enterOSLErrorScope();

  d_context->getShadingSystem().ShaderGroupEnd();

  return osl_error_scope.takeError();
}

} // End namespace llosl
