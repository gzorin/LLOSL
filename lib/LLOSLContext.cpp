#include <llosl/Builder.h>
#include <llosl/LLOSLContext.h>

#include <llvm/IR/LLVMContext.h>

#include "LLOSLContextImpl.h"

#include <map>
#include <utility>

namespace llosl {

// Implementation:
LLOSLContextImpl::LLOSLContextImpl(llvm::LLVMContext& llcontext)
    : d_llcontext(llcontext)
    , d_shading_system(new OSL::ShadingSystem(this, nullptr, &d_osl_error_handler))
    , d_shading_context(d_shading_system->get_context(d_shading_system->create_thread_info())) {
    d_shading_system->attribute("lockgeom", 0);
    d_shading_system->attribute("optimize", 0);
}

LLOSLContextImpl::~LLOSLContextImpl() {
    assert(!d_builder);
}

llvm::LLVMContext *
LLOSLContextImpl::llvm_context() const {
    return &d_llcontext;
}

OSLErrorScope
LLOSLContextImpl::enterOSLErrorScope() {
    return d_osl_error_handler.enter();
}

llvm::Expected<Builder>
LLOSLContextImpl::getBuilder() {
    if (d_builder) {
	return llvm::Expected<Builder>(
	    llvm::errorCodeToError(
		make_error_code(LLOSLContext::Error::AlreadyBuilding)));
    }

    return llvm::Expected<Builder>(Builder(*this));
}

void
LLOSLContextImpl::resetBuilder(BuilderImpl *builder) {
    d_builder = builder;
}

// Interface:
namespace {

namespace contexts {

std::map<llvm::LLVMContext*, LLOSLContext *>&
llvm_to_llosl() {
    static std::map<llvm::LLVMContext*, LLOSLContext *> s_llvm_to_llosl;
    return s_llvm_to_llosl;
}

} // End namespace contexts
} // End anonymous namespace

struct LLOSLContext::ErrorCategory : std::error_category {
    const char* name() const noexcept override {
	return "LLOSLContext";
    };

    std::string message(int ev) const override {
	switch (static_cast<Error>(ev)) {
	case Error::AlreadyBuilding:
	    return "already building";
	}
    }
};

const std::error_category&
LLOSLContext::ErrorCategory() {
    static struct ErrorCategory s_error_category;
    return s_error_category;
}

const LLOSLContext*
LLOSLContext::Get(const llvm::LLVMContext* llcontext) {
    auto it = contexts::llvm_to_llosl().find(const_cast<llvm::LLVMContext *>(llcontext));
    return it != contexts::llvm_to_llosl().end()?
	it->second :
	nullptr;
}

LLOSLContext*
LLOSLContext::Get(llvm::LLVMContext* llcontext) {
    auto it = contexts::llvm_to_llosl().find(llcontext);
    return it != contexts::llvm_to_llosl().end()?
	it->second :
	nullptr;
}

LLOSLContext::LLOSLContext(llvm::LLVMContext& llcontext)
  : d_impl(new LLOSLContextImpl(llcontext)) {
    contexts::llvm_to_llosl().insert(std::make_pair(&llcontext, this));
}

LLOSLContext::~LLOSLContext() {
    contexts::llvm_to_llosl().erase(&d_impl->getLLContext());
}

const llvm::LLVMContext&
LLOSLContext::getLLContext() const {
    return d_impl->getLLContext();
}

llvm::LLVMContext&
LLOSLContext::getLLContext() {
    return d_impl->getLLContext();
}

llvm::Expected<Builder>
LLOSLContext::getBuilder() {
    return d_impl->getBuilder();
}

} // End namespace llosl
