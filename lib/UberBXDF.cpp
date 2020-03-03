#include "LLOSLContextImpl.h"

#include <llosl/Closure.h>
#include <llosl/UberBXDF.h>

#include <llvm/IR/Constants.h>
#include <llvm/IR/Function.h>
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/Module.h>
#include <llvm/Transforms/Utils/Cloning.h>

#include <numeric>

namespace llosl {

UberBXDF::UberBXDF(LLOSLContextImpl &context)
    : d_context(&context) {
    d_context->addUberBXDF(this);

    auto &ll_context = d_context->getLLContext();

    d_module.reset(new llvm::Module("llosl-uber-bxdf", ll_context));

    // Copy the BXDF component declarations:
    std::for_each(context.closures().begin(), context.closures().end(),
                  [this, &ll_context](auto &tmp) -> void {
                      auto function = tmp.second->function();

                      d_bxdf_component_mapping[function] = llvm::Function::Create(
                          function->getFunctionType(), llvm::GlobalValue::ExternalLinkage,
                          function->getName(), d_module.get());
                  });

    auto float3_type = llvm::VectorType::get(llvm::Type::getFloatTy(ll_context), 3);

    auto void_pointer_type = llvm::Type::getInt8PtrTy(ll_context, 1);

    auto bxdf_type = llvm::FunctionType::get(
        float3_type,
        std::vector<llvm::Type *>{float3_type, float3_type, llvm::Type::getInt16Ty(ll_context),
                                  void_pointer_type},
        false);

    d_function = llvm::Function::Create(bxdf_type, llvm::GlobalValue::ExternalLinkage,
                                        "llosl_uber_bxdf", d_module.get());

    llvm::IRBuilder<> builder(ll_context);

    // Create entry_block, but don't populate it yet:
    auto entry_block = llvm::BasicBlock::Create(ll_context, "entry", d_function);

    // Create exit_block, which just returns some default value:
    auto exit_block = llvm::BasicBlock::Create(ll_context, "default_exit", d_function);
    builder.SetInsertPoint(exit_block);

    auto default_result = llvm::ConstantVector::getSplat(
        3, llvm::ConstantFP::get(llvm::Type::getFloatTy(ll_context), 0.0));
    builder.CreateRet(default_result);

    // Populate entry_block, which consists of a switch statement:
    builder.SetInsertPoint(entry_block);

    auto index           = d_function->arg_begin() + 2;
    d_switch_instruction = builder.CreateSwitch(index, exit_block, d_index.size());
}

UberBXDF::~UberBXDF() {}

std::pair<uint16_t, bool>
UberBXDF::insertBXDF(const BXDF *bxdf) {
    auto encoding = bxdf->encoding();

    auto it       = d_index.find(encoding);
    bool inserted = false;

    if (it == d_index.end()) {
        auto &ll_context = d_context->getLLContext();

        auto Wi    = d_function->arg_begin();
        auto Wr    = d_function->arg_begin() + 1;
        auto scope = d_function->arg_begin() + 3;

        // Clone the function that implements the BXDF:
        auto function = llvm::Function::Create(bxdf->function()->getFunctionType(),
                                               llvm::GlobalValue::ExternalLinkage,
                                               bxdf->function()->getName(), d_module.get());
        llvm::ValueToValueMapTy mapping;

        std::for_each(d_bxdf_component_mapping.begin(), d_bxdf_component_mapping.end(),
                      [&mapping](const auto &tmp) -> void {
                          mapping.insert({tmp.first, tmp.second});
                      });

        for (unsigned i = 0, n = bxdf->function()->arg_size(); i < n; ++i) {
            mapping[bxdf->function()->arg_begin() + i] = function->arg_begin() + i;
        }

        llvm::SmallVector<llvm::ReturnInst *, 1> returns;
        llvm::CloneFunctionInto(function, bxdf->function(), mapping, false, returns);

        // Create the block that gets branched to by the switch statement:
        llvm::IRBuilder<> builder(ll_context);

        auto block = llvm::BasicBlock::Create(ll_context, "", d_function);
        builder.SetInsertPoint(block);

        auto call_instruction = builder.CreateCall(
            function, std::vector<llvm::Value *>{
                          Wi, Wr,
                          builder.CreatePointerCast(
                              scope, llvm::PointerType::get(bxdf->scope_type(),
                                                            d_context->bxdf_address_space()))});

        builder.CreateRet(call_instruction);

        llvm::InlineFunctionInfo info;
        llvm::InlineFunction(call_instruction, info);
        function->eraseFromParent();

        // Add a case to the switch statement:
        uint16_t index = (uint16_t)d_index.size();

        d_switch_instruction->addCase(
            llvm::ConstantInt::get(llvm::Type::getInt16Ty(ll_context), index, false), block);

        //
        it       = d_index.insert({BXDF::Encoding(encoding), {bxdf, index, nullptr, block}}).first;
        inserted = true;
        d_max_heap_size = std::max(d_max_heap_size, bxdf->heap_size());
        d_modified      = true;
    }

    return {it->second.index, inserted};
}

const llvm::Function *
UberBXDF::getFunction() {
    if (!d_modified) {
        return d_function;
    }

    d_modified = false;

    return d_function;
}

} // End namespace llosl
