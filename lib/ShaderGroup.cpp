#include "BuilderImpl.h"
#include "LLOSLContextImpl.h"

#include <llosl/ShaderGroup.h>

#include <llvm/IR/Module.h>

#include <iostream>

namespace llosl {

ShaderGroup::ShaderGroup(BuilderImpl& builder) {
    auto& context = builder.context();
    context.addShaderGroup(this);

    auto& shading_system = context.getShadingSystem();
    auto shader_group = builder.shader_group();

    d_module = std::move(shading_system.get_group_module(shader_group.get()));
    //module->dump();
}

ShaderGroup::~ShaderGroup() {
}

} // End namespace llosl
