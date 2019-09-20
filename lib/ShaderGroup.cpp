#include "BuilderImpl.h"
#include "LLOSLContextImpl.h"

#include <llosl/IR/BXDFPass.h>
#include <llosl/IR/ClosureIRPass.h>
#include <llosl/IR/InstrumentationPass.h>
#include <llosl/IR/PathInfoPass.h>
#include <llosl/ShaderGroup.h>

#include <llvm/ADT/StringRef.h>
#include <llvm/IR/Constants.h>
#include <llvm/IR/DerivedTypes.h>
#include <llvm/IR/LegacyPassManager.h>
#include <llvm/IR/Metadata.h>
#include <llvm/IR/Module.h>
#include <llvm/Transforms/IPO.h>
#include <llvm/Transforms/Scalar.h>
#include <llvm/Transforms/Utils/Cloning.h>
#include <llvm/Transforms/Utils/UnifyFunctionExitNodes.h>

#include <iostream>

namespace llosl {

ShaderGroup::ShaderGroup(BuilderImpl& builder)
    : d_context(&builder.context()) {
    auto& ll_context = d_context->getLLContext();

    d_context->addShaderGroup(this);

    auto& shading_system = d_context->getShadingSystem();
    auto shader_group = builder.shader_group();

    d_module = std::move(shading_system.get_group_module(shader_group.get()));

    // Functions and types:
    //
    // Inline layers:
    auto main_function = shading_system.get_group_main_function(shader_group.get());
    auto main_function_name = main_function->getName();

    std::vector<llvm::CallInst *> call_instructions;

    std::for_each(
        main_function->begin(), main_function->end(),
        [&call_instructions](auto& block) -> void {
            std::for_each(
                block.begin(), block.end(),
                [&call_instructions](auto& instruction) -> void {
                    auto call_instruction = llvm::dyn_cast<llvm::CallInst>(&instruction);
                    if (!call_instruction) {
                        return;
                    }

                    call_instructions.push_back(call_instruction);
                });
        });

    std::for_each(
        call_instructions.begin(), call_instructions.end(),
        [](auto call_instruction) -> void {
            llvm::InlineFunctionInfo info;
            llvm::InlineFunction(call_instruction, info);
        });

    // Other optimizations:
    auto mpm = std::make_unique<llvm::legacy::PassManager>();
    mpm->add(llvm::createUnifyFunctionExitNodesPass());
    mpm->add(llvm::createReassociatePass());
    mpm->add(llvm::createSCCPPass());
    mpm->add(llvm::createAggressiveDCEPass());
    mpm->add(llvm::createCFGSimplificationPass());
    mpm->add(llvm::createPromoteMemoryToRegisterPass());
    mpm->run(*d_module);

    d_globals_type = shading_system.get_group_globals_type(shader_group.get());
    d_data_type = shading_system.get_group_data_type(shader_group.get());

    // Instrument the function with path information, and collect information
    // about the BXDFs:
    auto closure_ir = new ClosureIRPass();
    auto path_info = new PathInfoPass();
    auto instrumentation = new InstrumentationPass();
    auto bxdf = new BXDFPass();

    auto fpm = std::make_unique<llvm::legacy::FunctionPassManager>(d_module.get());
    fpm->add(closure_ir);
    fpm->add(path_info);
    fpm->add(instrumentation);
    fpm->add(bxdf);
    fpm->run(*main_function);

    main_function = d_module->getFunction(main_function_name);
    assert(main_function);

    d_init_function_md.reset(
        llvm::ValueAsMetadata::get(shading_system.get_group_init_function(shader_group.get())));
    d_main_function_md.reset(
        llvm::ValueAsMetadata::get(main_function));

    // Parameters:
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

    d_parameters_type = llvm::StructType::get(ll_context, parameter_ll_types);

    std::vector<llvm::Metadata *> parameter_mds;
    std::transform(
        d_parameters, d_parameters + d_parameter_count,
        std::back_inserter(parameter_mds),
            [](auto& parameter) -> llvm::Metadata * {
                return parameter.d_md.get();
        });

    d_parameters_md.reset(
        llvm::MDTuple::get(ll_context, parameter_mds));

    // BXDFs:
    d_bxdf_info = bxdf->getBXDFInfo();

    std::vector<llvm::Metadata *> bxdf_mds;
    auto it = std::back_inserter(bxdf_mds);

    // First node is the number of paths:
    unsigned path_count = d_bxdf_info->getPathCount();

    *it++ = llvm::ConstantAsMetadata::get(
        llvm::ConstantInt::get(ll_context, llvm::APInt(32, path_count)));

    // Second node is the max heap size:
    *it++ = llvm::ConstantAsMetadata::get(
        llvm::ConstantInt::get(ll_context, llvm::APInt(32, d_bxdf_info->getMaxHeapSize())));

    // Remaining nodes are repeating tuples of (path_id, heap_size, encoding):
    for (unsigned path_id = 0; path_id < path_count; ++path_id) {
        const auto& bxdf = d_bxdf_info->getBXDFForPath(path_id);
        auto encoding = BXDF::encode(bxdf.ast);

        *it++ = llvm::MDTuple::get(ll_context, {
            llvm::ConstantAsMetadata::get(
                llvm::ConstantInt::get(ll_context, llvm::APInt(32, path_id))),
            llvm::ConstantAsMetadata::get(
                llvm::ConstantInt::get(ll_context, llvm::APInt(32, bxdf.heap_size))),
            llvm::MDString::get(ll_context,
                llvm::StringRef(reinterpret_cast<const char *>(encoding.data()),
                                encoding.size()))
        });
    }

    d_bxdf_md.reset(
        llvm::MDTuple::get(ll_context, bxdf_mds));

    // All metadata:
    d_md.reset(
        llvm::MDTuple::get(ll_context, {
            d_init_function_md.get(),
            d_main_function_md.get(),
            d_parameters_md.get(),
            d_bxdf_md.get()
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

std::size_t
ShaderGroup::path_count() const {
    return d_bxdf_info->getPathCount();
}

const BXDF *
ShaderGroup::getBXDFForPath(std::size_t path_id) const {
    if (path_id >= path_count()) {
        return nullptr;
    }

    return &d_bxdf_info->getBXDFForPath(path_id);
}

std::size_t
ShaderGroup::getBXDFMaxHeapSize() const {
    return d_bxdf_info->getMaxHeapSize();
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
