#include "BuilderImpl.h"
#include "LLOSLContextImpl.h"

#include <llosl/ShaderGroup.h>

#include <llvm/IR/Constants.h>
#include <llvm/IR/DerivedTypes.h>
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

    int parameter_count = 0;
    OSL::ustring *parameter_names = nullptr;
    OSL::TypeDesc *parameter_types = nullptr;

    shading_system.getattribute(shader_group.get(), "num_userdata", parameter_count);
    shading_system.getattribute(shader_group.get(), "userdata_names", OSL::TypeDesc::PTR, &parameter_names);
    shading_system.getattribute(shader_group.get(), "userdata_types", OSL::TypeDesc::PTR, &parameter_types);

    d_parameter_count = parameter_count;
    d_parameters = std::allocator<Parameter>().allocate(d_parameter_count);

    auto parameter = d_parameters;
    std::vector<llvm::Type *> parameter_ll_types;

    for (unsigned index = 0; parameter_count > 0; --parameter_count, ++index) {
        new (parameter++) Parameter(index, *parameter_names++, *parameter_types++, this);
        parameter_ll_types.push_back(d_data_type->getStructElementType(index));
    }

    auto& ll_context = d_context->getLLContext();

    d_parameters_type = llvm::StructType::get(ll_context, parameter_ll_types);

    std::vector<llvm::Metadata *> mds;
    std::transform(
        d_parameters, d_parameters + d_parameter_count,
        std::back_inserter(mds),
            [](auto& parameter) -> llvm::Metadata * {
                return parameter.d_md.get();
        });

    d_parameters_md.reset(
        llvm::MDTuple::get(ll_context, mds));

    d_md.reset(
        llvm::MDTuple::get(ll_context, {
            d_init_function_md.get(),
            d_main_function_md.get(),
            d_parameters_md.get()
        }));

    auto shadergroups_md = d_module->getOrInsertNamedMetadata("llosl.shadergroups");
    shadergroups_md->addOperand(d_md.get());
}

ShaderGroup::~ShaderGroup() {
    if (d_context) {
	    d_context->removeShaderGroup(this);
    }

    std::for_each(
        d_parameters, d_parameters+d_parameter_count,
        [](auto& parameter) -> void {
	        parameter.~Parameter();
        });

    std::allocator<Parameter>().deallocate(d_parameters, d_parameter_count);
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

ShaderGroup::Parameter::Parameter(unsigned index, const OSL::ustring& name, const OSL::TypeDesc& type, ShaderGroup *parent)
: d_parent(parent) {
    auto& ll_context = d_parent->module()->getContext();

    d_md.reset(
        llvm::MDTuple::get(
        ll_context, {
            llvm::ConstantAsMetadata::get(
                llvm::ConstantInt::get(ll_context, llvm::APInt(32, index, true))),
            llvm::MDString::get(ll_context, "llosl.parameter_name"),
            llvm::MDString::get(ll_context, name.c_str()),
            llvm::MDString::get(ll_context, "llosl.type"),
            llvm::ConstantAsMetadata::get(
                llvm::ConstantInt::get(ll_context, llvm::APInt(8, type.basetype, true))),
            llvm::ConstantAsMetadata::get(
                llvm::ConstantInt::get(ll_context, llvm::APInt(8, type.aggregate, true))),
            llvm::ConstantAsMetadata::get(
                llvm::ConstantInt::get(ll_context, llvm::APInt(8, type.vecsemantics, true))),
            llvm::ConstantAsMetadata::get(
                llvm::ConstantInt::get(ll_context, llvm::APInt(32, type.arraylen, true)))
        }));
}

unsigned
ShaderGroup::Parameter::index() const {
    auto md = llvm::mdconst::dyn_extract<llvm::ConstantInt>(d_md->getOperand(0).get());
    assert(md);

	return md->getZExtValue();
}

llvm::StringRef
ShaderGroup::Parameter::name() const {
    return llvm::cast<llvm::MDString>(d_md->getOperand(2).get())->
	    getString();
}

llvm::Type *
ShaderGroup::Parameter::llvm_type() const {
    return llvm::cast<llvm::StructType>(d_parent->data_type())->getTypeAtIndex(index());
}

OIIO::TypeDesc
ShaderGroup::Parameter::osl_type() const {
    auto basetype_md = llvm::mdconst::dyn_extract<llvm::ConstantInt>(d_md->getOperand(4).get());
    assert(basetype_md);

    auto aggregate_md = llvm::mdconst::dyn_extract<llvm::ConstantInt>(d_md->getOperand(5).get());
    assert(aggregate_md);

    auto vecsemantics_md = llvm::mdconst::dyn_extract<llvm::ConstantInt>(d_md->getOperand(6).get());
    assert(vecsemantics_md);

    auto arraylen_md = llvm::mdconst::dyn_extract<llvm::ConstantInt>(d_md->getOperand(7).get());
    assert(arraylen_md);

    return OIIO::TypeDesc(
        static_cast<OIIO::TypeDesc::BASETYPE>(basetype_md->getZExtValue()),
        static_cast<OIIO::TypeDesc::AGGREGATE>(aggregate_md->getZExtValue()),
        static_cast<OIIO::TypeDesc::VECSEMANTICS>(vecsemantics_md->getZExtValue()),
        static_cast<unsigned>(arraylen_md->getZExtValue())
    );
}

} // End namespace llosl
