#include "OSLErrorHandler.h"

namespace llosl {

namespace {

struct ErrorCategory : std::error_category {
    static const ErrorCategory& Get() {
	static ErrorCategory s_error_category;
	return s_error_category;
    }

    const char *name() const noexcept override {
	return "OSL";
    };

    std::string message(int code) const override {
	return std::string("OSL error code ") + std::to_string(code);
    };
};

} // End anonymous namespace

char OSLError::ID = 0;

OSLError::OSLError(int code, const std::string& message)
    : d_code(code)
    , d_message(message) {
}

void
OSLError::log(llvm::raw_ostream& os) const {
    os << d_message;
}

std::error_code OSLError::convertToErrorCode() const {
    return std::error_code(static_cast<int>(d_code), ErrorCategory::Get());
}

OSLErrorScope::OSLErrorScope(OSLErrorHandler *context)
    : d_context(context)
    , d_checked(false) {
    d_context->d_scopes.push(this);
}

OSLErrorScope::OSLErrorScope(OSLErrorScope&& rhs)
    : d_context(nullptr)
    , d_checked(false)
    , d_code(0) {
    *this = std::move(rhs);
}
    
OSLErrorScope::~OSLErrorScope()
{
    if (d_context) {
	d_context->d_scopes.pop();
    }

    assert(d_checked);
}

OSLErrorScope&
OSLErrorScope::operator =(OSLErrorScope&& rhs) {
    std::swap(d_context, rhs.d_context);
    std::swap(d_code, rhs.d_code);

    d_checked = false;
    rhs.d_checked = true;

    std::swap(d_message, rhs.d_message);

    d_context->d_scopes.top() = this;

    return *this;
}

llvm::Error
OSLErrorScope::takeError() {
    d_checked = true;

    if (d_code == 0) {
	return llvm::Error::success();
    }

    std::unique_ptr<OSLError> payload(new OSLError(d_code, d_message));
    return llvm::Error(std::move(payload));
}

OSLErrorScope
OSLErrorHandler::enter()
{
    OSLErrorScope result(this);
    return result;
}

void
OSLErrorHandler::operator()(int errcode, const std::string& msg) {
    if (d_scopes.empty()) {
	return;
    }

    auto errcode_high = errcode >> 16;

    if (errcode_high != OSL::ErrorHandler::EH_ERROR &&
	errcode_high != OSL::ErrorHandler::EH_SEVERE) {
	return;
    }

    d_scopes.top()->d_code = false;
    d_scopes.top()->d_code = errcode;
    d_scopes.top()->d_message = msg;
}

} // End namespace llosl
