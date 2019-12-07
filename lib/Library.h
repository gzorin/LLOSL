//-*-C++-*-
#ifndef LLOSL_LIBRARY_H
#define LLOSL_LIBRARY_H

#include "LLOSLContextImpl.h"
#include "StringUtil.h"

#include <llvm/ADT/StringRef.h>
#include <llvm/IR/IRBuilder.h>

#include <osl_pvt.h>

#include <map>

namespace llosl {

class LLOSLContextImpl;
class SymbolScope;
class TypeScope;

class Library {
public:

    Library(LLOSLContextImpl&, TypeScope&, llvm::Module&);

    struct Function {
        OSL::pvt::TypeSpec returnType;
        std::vector<OSL::pvt::TypeSpec> paramTypes;
        bool isVarArg = false;

        llvm::Function *function = nullptr;
    };

    Function *declare(const char *, const char *);

    Function& operator[](llvm::StringRef);
    Function& operator[](OSL::ustring);

    class CallingContext {
    public:

        CallingContext(Library&, SymbolScope&, llvm::IRBuilder<>&);

        llvm::Value *call(llvm::StringRef, llvm::ArrayRef<llvm::Value *> = llvm::None);
        llvm::Value *call(Function&, llvm::ArrayRef<llvm::Value *> = llvm::None);

        template<typename... Args>
        llvm::Value *operator()(llvm::StringRef, Args&&...);

        template<typename... Args>
        llvm::Value *operator()(Function&, Args&&...);

        class Callable {
        public:

            template<typename... Args>
            llvm::Value *operator()(Args&&...);

        private:

            Callable(CallingContext&, Function&);

            CallingContext& d_caller;
            Function& d_function;

            friend class CallingContext;
        };

        Callable operator[](llvm::StringRef);
        Callable operator[](OSL::ustring);

    private:

        static llvm::Value *GetValueForArgument(SymbolScope&, const Symbol *);

        template<typename... Args>
        llvm::Value *callv(Function&, Args&&...);

        Library& d_library;
        SymbolScope& d_symbol_scope;
        llvm::IRBuilder<>& d_builder;
    };

private:

    LLOSLContextImpl& d_context;
    TypeScope& d_type_scope;
    llvm::Module& d_module;

    std::map<OSL::ustring, Function> d_functions;
};

template<typename... Args>
llvm::Value *
Library::CallingContext::operator()(llvm::StringRef name, Args&&... args) {
    auto it = d_library.d_functions.find(makeOSLUString(name));
    assert (it != d_library.d_functions.end());

    return callv(it->second, std::forward<Args>(args)...);
}

template<typename... Args>
llvm::Value *
Library::CallingContext::operator()(Function& function, Args&&... args) {
    return callv(function, std::forward<Args>(args)...);
}

namespace {

template<typename Collector, typename... Args>
void
CollectArguments(Collector&, Args&&...);

template<typename Collector, typename T>
void
CollectArguments(Collector& collector, T&& arg) {
    collector(arg);
}

template<typename Collector, typename T, typename... Args>
void
CollectArguments(Collector& collector, T&& head, Args&&... tail) {
    collector(head);
    return CollectArguments(collector, std::forward<Args>(tail)...);
}

}

template<typename... Args>
llvm::Value *
Library::CallingContext::callv(Function& function, Args&&... args) {
    struct Collector {
        Collector(CallingContext& caller, Function& function)
            : d_caller(caller)
            , d_function(function)
            , d_llcontext(d_caller.d_library.d_context.getLLContext())
            , d_type_scope(d_caller.d_library.d_type_scope)
            , d_symbol_scope(d_caller.d_symbol_scope) {
            d_args.reserve(function.paramTypes.size());
        }

        llvm::Value *
        finalize() {
            return d_caller.call(d_function, d_args);
        }

        void operator()(const Symbol *s) {
            add(GetValueForArgument(d_symbol_scope, s));
        }

        void operator()(int arg) {
            add(llvm::ConstantInt::get(d_llcontext, llvm::APInt(32, arg, true)));
        }

        void operator()(llvm::Value* arg) {
            add(arg);
        }

    private:

        void add(llvm::Value* arg) {
            d_args.push_back(arg);
        }

        CallingContext& d_caller;
        Function& d_function;
        llvm::LLVMContext& d_llcontext;
        TypeScope& d_type_scope;
        SymbolScope& d_symbol_scope;

        std::vector<llvm::Value *> d_args;
    };

    Collector collector(*this, function);
    CollectArguments(collector, std::forward<Args>(args)...);
    return collector.finalize();
}

template<typename... Args>
llvm::Value *
Library::CallingContext::Callable::operator()(Args&&... args) {
    return d_caller.callv(d_function, std::forward<Args>(args)...);
}

} // End namespace llosl

#endif