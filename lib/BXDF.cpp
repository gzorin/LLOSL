#include "LLOSLContextImpl.h"

#include <llosl/BXDF.h>
#include <llosl/IR/BXDFAST.h>

#include <llvm/IR/Function.h>
#include <llvm/IR/IRBuilder.h>
#include <llvm/Support/raw_ostream.h>

#include <unordered_map>

namespace llosl {

BXDF::BXDF(LLOSLContextImpl& context, EncodingView encoding, BXDFAST::NodeRef ast, std::size_t heap_size)
: d_context(&context)
, d_encoding(encoding)
, d_heap_size(heap_size) {
    d_context->addBXDF(this);

    auto& ll_context = d_context->getLLContext();

    // Collect the BXDF params type:
    std::vector<llvm::Type *> scope_types;

    { struct PreVisitor {
        void operator()(BXDFAST::NodeRef, const BXDFAST::Void&) {}
        void operator()(BXDFAST::NodeRef, const BXDFAST::Component&) {}
        void operator()(BXDFAST::NodeRef, const BXDFAST::WeightedComponent&) {}
        void operator()(BXDFAST::NodeRef, const BXDFAST::Add&) {}
        void operator()(BXDFAST::NodeRef, const BXDFAST::MulColor&) {}
        void operator()(BXDFAST::NodeRef, const BXDFAST::MulFloat&) {}
      };

      struct PostVisitor {
        LLOSLContextImpl& context;
        llvm::LLVMContext& ll_context;
        std::vector<llvm::Type *>& scope_types;

        void operator()(BXDFAST::NodeRef ref, const BXDFAST::Void&) {}

        void operator()(BXDFAST::NodeRef ref, const BXDFAST::Component& node) {
            int id = node.id;
            llvm::Type *params_type = nullptr;
            context.getShadingSystem().query_closure(
                nullptr, &id, nullptr, &params_type);
            scope_types.push_back(params_type);
        }

        void operator()(BXDFAST::NodeRef, const BXDFAST::WeightedComponent& node) {
            int id = node.id;
            llvm::Type *params_type = nullptr;
            context.getShadingSystem().query_closure(
                nullptr, &id, nullptr, &params_type);
            scope_types.push_back(params_type);
        }

        void operator()(BXDFAST::NodeRef ref, const BXDFAST::Add&) {}

        void operator()(BXDFAST::NodeRef ref, const BXDFAST::MulColor&) {
            scope_types.push_back(
                llvm::VectorType::get(
                    llvm::Type::getFloatTy(ll_context), 3));
        }

        void operator()(BXDFAST::NodeRef ref, const BXDFAST::MulFloat&) {
            scope_types.push_back(
                llvm::Type::getFloatTy(ll_context));
        }
      };

      PreVisitor pre;
      PostVisitor post = { *d_context, ll_context, scope_types };
      BXDFAST::visit(ast, pre, post);
    }

    d_scope_type = llvm::StructType::get(ll_context, scope_types);

    auto float3_type = llvm::VectorType::get(
        llvm::Type::getFloatTy(ll_context), 3);

    auto void_pointer_type = llvm::Type::getInt8PtrTy(
        ll_context);

    auto bxdf_type = llvm::FunctionType::get(
        float3_type,
        std::vector<llvm::Type *>{
            float3_type, float3_type,
            llvm::PointerType::get(d_scope_type, 0) },
        false);

    d_function = llvm::Function::Create(
        bxdf_type, llvm::GlobalValue::ExternalLinkage, "llosl_bxdf", d_context->bxdf_module());

    auto Wi    = d_function->arg_begin();
    auto Wr    = d_function->arg_begin() + 1;
    auto scope = d_function->arg_begin() + 2;
    llvm::Value *result = nullptr;

    auto block = llvm::BasicBlock::Create(ll_context, "", d_function);
    llvm::IRBuilder<> builder(ll_context);
    builder.SetInsertPoint(block);

    { struct PreVisitor {
        void operator()(BXDFAST::NodeRef, const BXDFAST::Void&) {}
        void operator()(BXDFAST::NodeRef, const BXDFAST::Component&) {}
        void operator()(BXDFAST::NodeRef, const BXDFAST::WeightedComponent&) {}
        void operator()(BXDFAST::NodeRef, const BXDFAST::Add&) {}
        void operator()(BXDFAST::NodeRef, const BXDFAST::MulColor&) {}
        void operator()(BXDFAST::NodeRef, const BXDFAST::MulFloat&) {}
      };

      struct PostVisitor {
        LLOSLContextImpl& context;
        llvm::LLVMContext& ll_context;
        llvm::IRBuilder<>& builder;
        llvm::Value *&result;
        llvm::Value *Wi, *Wr, *scope;
        unsigned scope_index = 0;
        std::unordered_map<BXDFAST::NodeRef, llvm::Value *> values;

        void operator()(BXDFAST::NodeRef, const BXDFAST::Void&) {}

        void operator()(BXDFAST::NodeRef ref, const BXDFAST::Component& node) {
            auto args = builder.CreateStructGEP(nullptr, scope, scope_index++);
            result = builder.CreateCall(
                context.getBXDFComponent(node.id),
                std::vector<llvm::Value*>{ Wi, Wr, args });

            values[ref] = result;
        }

        void operator()(BXDFAST::NodeRef, const BXDFAST::WeightedComponent&) {
        }

        void operator()(BXDFAST::NodeRef ref, const BXDFAST::Add& node) {
            auto lhs = values[node.lhs];
            auto rhs = values[node.rhs];

            result = builder.CreateFAdd(lhs, rhs);

            values[ref] = result;
        }

        void operator()(BXDFAST::NodeRef ref, const BXDFAST::MulColor& node) {
            auto lhs = values[node.lhs];
            auto rhs = builder.CreateLoad(
                builder.CreateStructGEP(nullptr, scope, scope_index++));

            result = builder.CreateFMul(lhs, rhs);

            values[ref] = result;
        }

        void operator()(BXDFAST::NodeRef ref, const BXDFAST::MulFloat& node) {
            auto lhs = values[node.lhs];
            auto rhs = builder.CreateLoad(
                builder.CreateStructGEP(nullptr, scope, scope_index++));

            result = builder.CreateFMul(
                lhs, builder.CreateInsertElement(
                    builder.CreateInsertElement(
                        builder.CreateInsertElement(
                            llvm::UndefValue::get(llvm::VectorType::get(
                                                llvm::Type::getFloatTy(ll_context), 3)),
                            rhs, (unsigned)0), rhs, (unsigned)1), rhs, (unsigned)2));

            values[ref] = result;
        }
      };

      PreVisitor pre;
      PostVisitor post = { *d_context, ll_context, builder, result, Wi, Wr, scope };
      BXDFAST::visit(ast, pre, post);
      builder.CreateRet(post.result);
    }
}

BXDF::~BXDF() {
}

} // End namespace llosl
