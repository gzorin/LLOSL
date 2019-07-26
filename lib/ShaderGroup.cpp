#include "BuilderImpl.h"
#include "LLOSLContextImpl.h"

#include <llosl/ShaderGroup.h>

#include <llvm/IR/Module.h>

#include <iostream>

namespace llosl {

ShaderGroup::ShaderGroup(BuilderImpl& builder)
    : d_context(&builder.context()) {
    d_context->addShaderGroup(this);

    auto& shading_system = d_context->getShadingSystem();
    auto shader_group = builder.shader_group();

    d_module = std::move(shading_system.get_group_module(shader_group.get()));
}

ShaderGroup::~ShaderGroup() {
    if (d_context) {
	d_context->removeShaderGroup(this);
    }
}

void
ShaderGroup::removeFromContext() {
    if (d_context) {
	d_context->removeShaderGroup(this);
	d_context = nullptr;
    }
}

} // End namespace llosl
