//-*-C++-*-
#ifndef LLOSL_SYMBOLSCOPE_H
#define LLOSL_SYMBOLSCOPE_H

#include "Library.h"

#include <llvm/ADT/DenseMap.h>
#include <llvm/IR/IRBuilder.h>

#include <osl_pvt.h>

#include <map>
#include <memory>

namespace llosl {

class LLOSLContextImpl;
class TypeScope;

using ShaderGlobalsIndex = std::map<OSL::ustring, unsigned>;

const ShaderGlobalsIndex&
getShaderGlobalsIndex();

class SymbolScope {
public:

    SymbolScope(LLOSLContextImpl&, TypeScope&, Library&, llvm::IRBuilder<>& builder,
                llvm::Function * = nullptr, llvm::Value * = nullptr);

    void add(const Symbol *);

    llvm::Value *getReference(const Symbol *);
    llvm::Value *getValue(const Symbol *);
    llvm::Value *getValueOrDereference(const Symbol *);
    llvm::Value *getValueForArgument(const Symbol *);

private:

    void addReference(const Symbol *, llvm::Value *);
    void addValue(const Symbol *, llvm::Value *);
    llvm::Value *allocate(const Symbol *);

    LLOSLContextImpl& d_context;
    TypeScope& d_type_scope;
    Library::CallingContext d_library_context;
    llvm::IRBuilder<>& d_builder;
    llvm::Function *d_function = nullptr;
    llvm::Value *d_shader_globals = nullptr;
    Library::CallingContext::Callable llosl_closure_output_annotation, llosl_closure_storage_annotation;

    std::map<OSL::ustring, llvm::Argument *> d_arguments_by_name;

    llvm::DenseMap<const Symbol *, llvm::Value *> d_references, d_values;
};

} // End namespace llosl

#endif
