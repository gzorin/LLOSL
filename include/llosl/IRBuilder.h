//-*-C++-*-
#ifndef LLOSL_IRBUILDER_H
#define LLOSL_IRBUILDER_H

#include <llvm/IR/DerivedTypes.h>
#include <llvm/IR/IRBuilder.h>

namespace llosl {

template <typename T = llvm::ConstantFolder, typename Inserter = llvm::IRBuilderDefaultInserter>
llvm::Value *
CreateLoadPacked(llvm::IRBuilder<T, Inserter> &builder, llvm::Value *address) {
    assert(address->getType()->isPointerTy());
    assert(address->getType()->getPointerElementType()->isArrayTy());

    auto array_type = llvm::cast<llvm::ArrayType>(address->getType()->getPointerElementType());
    auto vector_type =
        llvm::VectorType::get(array_type->getElementType(), array_type->getNumElements());

    llvm::Value *result = llvm::UndefValue::get(vector_type);

    for (unsigned i = 0, n = array_type->getNumElements(); i < n; ++i) {
        result = builder.CreateInsertElement(
            result, builder.CreateLoad(builder.CreateConstGEP2_32(nullptr, address, 0, i)), i);
    }

    return result;
}

template <typename T = llvm::ConstantFolder, typename Inserter = llvm::IRBuilderDefaultInserter>
void
CreateStorePacked(llvm::IRBuilder<T, Inserter> &builder, llvm::Value *value, llvm::Value *address) {
    assert(value->getType()->isVectorTy());
    assert(address->getType()->isPointerTy());
    assert(address->getType()->getPointerElementType()->isArrayTy());

    auto array_type = llvm::cast<llvm::ArrayType>(address->getType()->getPointerElementType());

    for (unsigned i = 0, n = array_type->getNumElements(); i < n; ++i) {
        builder.CreateStore(builder.CreateExtractElement(value, i),
                            builder.CreateConstGEP2_32(nullptr, address, 0, i));
    }
}

} // End namespace llosl

#endif