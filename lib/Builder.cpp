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

class BuilderImpl {
public:

    using Error = Builder::Error;

    BuilderImpl(LLOSLContextImpl&);
    ~BuilderImpl();

    BuilderImpl() = delete;
    BuilderImpl(const BuilderImpl&) = delete;
    BuilderImpl(BuilderImpl&&) = delete;

    llvm::Error BeginShaderGroup(llvm::StringRef, llvm::StringRef);
    llvm::Error EndShaderGroup();

    llvm::Error AddNode(llvm::StringRef, llvm::StringRef, llvm::StringRef);

private:

    enum class State {
	kValid, kInvalidContext, kInvalidError
    };
    
    State d_state = State::kValid;
    LLOSLContextImpl *d_context;

    OSL::ShaderGroupRef d_shader_group;
};

BuilderImpl::BuilderImpl(LLOSLContextImpl& context)
    : d_context(&context)
    , d_state(State::kValid) {
    d_context->resetBuilder(this);
}

BuilderImpl::~BuilderImpl() {
    d_context->resetBuilder(nullptr);
}

llvm::Error
BuilderImpl::BeginShaderGroup(llvm::StringRef name, llvm::StringRef usage) {
  switch(d_state) {
  case State::kInvalidContext:
      return llvm::make_error<Error>(Error::Code::InvalidContext);
  case State::kInvalidError:
      return llvm::make_error<Error>(Error::Code::InvalidError);
  default:
      break;
  }

  auto osl_error_scope = d_context->enterOSLErrorScope();

  d_shader_group = d_context->getShadingSystem().ShaderGroupBegin(ConvertStringRef(name), ConvertStringRef(usage));

  return osl_error_scope.takeError();
}

llvm::Error
BuilderImpl::EndShaderGroup() {
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

  auto error = osl_error_scope.takeError();

  if (error) {
      d_state = State::kInvalidError;
  }

  return std::move(error);
}

llvm::Error
BuilderImpl::AddNode(llvm::StringRef usage, llvm::StringRef shadername, llvm::StringRef layername) {
  switch(d_state) {
  case State::kInvalidContext:
      return llvm::make_error<Error>(Error::Code::InvalidContext);
  case State::kInvalidError:
      return llvm::make_error<Error>(Error::Code::InvalidError);
  default:
      break;
  }

  auto osl_error_scope = d_context->enterOSLErrorScope();

  d_context->getShadingSystem().Shader(
      ConvertStringRef(usage), ConvertStringRef(shadername), ConvertStringRef(layername));

  auto error = osl_error_scope.takeError();

  if (error) {
      d_state = State::kInvalidError;
  }

  return std::move(error);
}

Builder::Builder(LLOSLContextImpl& context)
    : d_impl(std::make_unique<BuilderImpl>(context)) {
}

Builder::Builder(Builder&& rhs) {
    *this = std::move(rhs);
}

Builder::~Builder() {
}

Builder&
Builder::operator =(Builder&& rhs) {
    std::swap(d_impl, rhs.d_impl);
    return *this;
}

llvm::Error
Builder::BeginShaderGroup(llvm::StringRef name, llvm::StringRef usage) {
    return d_impl->BeginShaderGroup(name, usage);
}

llvm::Error
Builder::EndShaderGroup() {
    return d_impl->EndShaderGroup();
}

llvm::Error
Builder::AddNode(llvm::StringRef usage, llvm::StringRef shadername, llvm::StringRef layername) {
    return d_impl->AddNode(usage, shadername, layername);
}

} // End namespace llosl
