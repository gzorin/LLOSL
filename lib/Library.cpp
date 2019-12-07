//-*-C++-*-
#include "Library.h"
#include "LLOSLContextImpl.h"
#include "SymbolScope.h"
#include "TypeScope.h"

#include <llvm/IR/Function.h>
#include <llvm/IR/Module.h>

#include <oslcomp_pvt.h>

namespace llosl {

Library::Library(LLOSLContextImpl& context, TypeScope& type_scope, llvm::Module& module)
    : d_context(context)
    , d_type_scope(type_scope)
    , d_module(module) {
}

Library::Function *
Library::declare(const char *name, const char *signature) {
    auto tmp = d_functions.insert({ OSL::ustring(name), Function() });
    assert(tmp.second);

    auto it = tmp.first;

    auto& function = it->second;

    auto parseType = [this](const char *signature) -> std::tuple<OSL::pvt::TypeSpec, const char *> {
        int advance = 0;
        auto t = OSLCompilerImpl::type_from_code(signature, &advance);

        return { t, signature + advance };
    };

    //
    auto [ t, next_signature ] = parseType(signature);
    std::exchange(signature, next_signature);

    function.returnType = t;

    while (*signature) {
        auto [ t, next_signature ] = parseType(signature);
        auto t_signature = std::exchange(signature, next_signature);

        if (*t_signature == '*') {
            assert(t.simpletype().basetype == TypeDesc::UNKNOWN);
            function.isVarArg = true;
            continue;
        }

        function.paramTypes.push_back(t);
    }

    //
    auto return_type = d_type_scope.get(function.returnType);

    std::vector<llvm::Type *> param_types;
    param_types.reserve(function.paramTypes.size());

    std::transform(
        function.paramTypes.begin(), function.paramTypes.end(),
        std::back_inserter(param_types),
        [this](const auto& t) -> auto {
            return d_type_scope.getForArgument(t);
        });

    function.function = llvm::Function::Create(
        llvm::FunctionType::get(return_type, param_types, function.isVarArg),
        llvm::GlobalValue::ExternalLinkage, name, &d_module);

    return &it->second;
}

Library::Function&
Library::operator[](llvm::StringRef name) {
    auto it = d_functions.find(makeOSLUString(name));
    assert(it != d_functions.end());

    return it->second;
}

Library::Function&
Library::operator[](OSL::ustring name) {
    auto it = d_functions.find(name);
    assert(it != d_functions.end());

    return it->second;
}

llvm::Value *
Library::CallingContext::GetValueForArgument(SymbolScope& symbol_scope, const Symbol * s) {
    return symbol_scope.getValueForArgument(s);
}

Library::CallingContext::CallingContext(Library& library, SymbolScope& symbol_scope, llvm::IRBuilder<>& builder)
    : d_library(library)
    , d_symbol_scope(symbol_scope)
    , d_builder(builder) {
}

llvm::Value *
Library::CallingContext::call(llvm::StringRef name, llvm::ArrayRef<llvm::Value *> args) {
    auto it = d_library.d_functions.find(makeOSLUString(name));
    assert (it != d_library.d_functions.end());

    return call(it->second, args);
}

llvm::Value *
Library::CallingContext::call(Function& function, llvm::ArrayRef<llvm::Value *> args) {
    return d_builder.CreateCall(function.function, args);
}

Library::CallingContext::Callable
Library::CallingContext::operator[](llvm::StringRef name) {
    return Callable(*this, d_library[name]);
}

Library::CallingContext::Callable
Library::CallingContext::operator[](OSL::ustring name) {
    return Callable(*this, d_library[name]);
}

Library::CallingContext::Callable::Callable(CallingContext& caller, Function& function)
    : d_caller(caller)
    , d_function(function) {
}

} // End namespace llosl