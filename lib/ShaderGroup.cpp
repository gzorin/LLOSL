#include "BuilderImpl.h"
#include "LLOSLContextImpl.h"

#include <llosl/ShaderGroup.h>

#include <llvm/IR/Metadata.h>
#include <llvm/IR/Module.h>

#include <iostream>

namespace llosl {

ShaderGroup::ShaderGroup(BuilderImpl& builder)
    : d_context(&builder.context()) {
    d_context->addShaderGroup(this);

    auto& shading_system = d_context->getShadingSystem();
    auto shader_group = builder.shader_group();

    d_module = std::move(shading_system.get_group_module(shader_group.get()));
    d_globals_type = shading_system.get_group_globals_type(shader_group.get());
    d_data_type = shading_system.get_group_data_type(shader_group.get());

    d_init_function_md.reset(
        llvm::ValueAsMetadata::get(shading_system.get_group_init_function(shader_group.get())));
    d_main_function_md.reset(
        llvm::ValueAsMetadata::get(shading_system.get_group_main_function(shader_group.get())));

    d_md.reset(
      llvm::MDTuple::get(d_context->getLLContext(), {
	      d_init_function_md.get(),
	      d_main_function_md.get()
	  }));

    auto shadergroups_md = d_module->getOrInsertNamedMetadata("llosl.shadergroups");
    shadergroups_md->addOperand(d_md.get());
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

const llvm::Function *
ShaderGroup::init_function() const {
    return d_init_function_md
      ? llvm::cast<llvm::Function>(d_init_function_md->getValue())
      : nullptr;
}

const llvm::Function *
ShaderGroup::main_function() const {
    return d_main_function_md
      ? llvm::cast<llvm::Function>(d_main_function_md->getValue())
      : nullptr;
}


} // End namespace llosl
