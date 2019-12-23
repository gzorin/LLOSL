#include "LLOSLContextImpl.h"

#include <llosl/Closure.h>

#include <llvm/IR/DerivedTypes.h>
#include <llvm/IR/Function.h>
#include <llvm/Support/FormatVariadic.h>

#include <algorithm>

namespace llosl {

std::unique_ptr<Closure>
Closure::create(LLOSLContext& context, llvm::StringRef name, unsigned id, llvm::ArrayRef<OSL::ClosureParam> params, llvm::Module *module) {
    return create(LLOSLContextImpl::Get(context), name, id, params, module);
}

std::unique_ptr<Closure>
Closure::create(LLOSLContextImpl& context_impl, llvm::StringRef name, unsigned id, llvm::ArrayRef<OSL::ClosureParam> params, llvm::Module *module) {
    auto closure = std::make_unique<Closure>();

    closure->d_name = name;
    closure->d_id = id;

    closure->d_param_list.reserve(params.size());

    std:transform(
        params.begin(), params.end(),
        std::back_inserter(closure->d_param_list),
        [](const auto& param) -> OSL::TypeDesc {
            return param.type;
        });

    std::vector<llvm::Type *> param_types;
    param_types.reserve(closure->d_param_list.size());

    std::transform(
        closure->d_param_list.begin(), closure->d_param_list.end(),
        std::back_inserter(param_types),
        [&context_impl](const auto& t) -> llvm::Type * {
            return context_impl.getLLVMType(t, true);
        });

    auto params_type_name = llvm::formatv("struct.OSL::{0}", closure->d_name).str();
    closure->d_params_type = llvm::StructType::create(context_impl.getLLContext(), param_types, params_type_name);

    //
    auto vector_type = context_impl.getLLVMType(OSL::TypeDesc(OSL::TypeDesc::FLOAT, OSL::TypeDesc::VEC3), false);

    auto function_type = llvm::FunctionType::get(
        vector_type,
        std::vector<llvm::Type *>{
            vector_type, vector_type,
            llvm::PointerType::get(closure->d_params_type, context_impl.bxdf_address_space()) },
        false);

    auto function_name = llvm::formatv("llosl_{0}", closure->d_name).str();

    closure->d_function = llvm::Function::Create(
        function_type, llvm::GlobalValue::ExternalLinkage,
        function_name, module);

    return closure;
}

Closure::Closure() {
}

} // End namespace llosl