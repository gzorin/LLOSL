//-*-C++-*-

#include "Library.h"
#include "LLOSLContextImpl.h"
#include "StringUtil.h"
#include "SymbolScope.h"
#include "TypeScope.h"

namespace llosl {

const ShaderGlobalsIndex&
getShaderGlobalsIndex() {
    static ShaderGlobalsIndex index = {
        { OSL::ustring("P"),              0 },
        { OSL::ustring("I"),              4 },
        { OSL::ustring("N"),              7 },
        { OSL::ustring("Ng"),             8 },
        { OSL::ustring("u"),              9 },
        { OSL::ustring("v"),             12 },
        { OSL::ustring("dPdu"),          15 },
        { OSL::ustring("dPdv"),          16 },
        { OSL::ustring("time"),          17 },
        { OSL::ustring("dtime"),         18 },
        { OSL::ustring("dPdtime"),       19 },
        { OSL::ustring("renderer"),      20 },
        { OSL::ustring("object2common"), 21 },
        { OSL::ustring("shader2common"), 22 },
        { OSL::ustring("Ci"),            23 },
    };

    return index;
}

SymbolScope::SymbolScope(LLOSLContextImpl& context, TypeScope& type_scope, Library& library, llvm::IRBuilder<>& builder,
                         llvm::Function *function, llvm::Value *shader_globals)
    : d_context(context)
    , d_type_scope(type_scope)
    , d_library_context(library, *this, builder)
    , d_builder(builder)
    , d_function(function)
    , d_shader_globals(shader_globals)
    , llosl_closure_Ci_annotation(d_library_context["llosl_closure_Ci_annotation"])
    , llosl_closure_output_annotation(d_library_context["llosl_closure_output_annotation"])
    , llosl_closure_storage_annotation(d_library_context["llosl_closure_storage_annotation"]) {

    if (d_function) {
        std::for_each(
            d_function->arg_begin(), d_function->arg_end(),
            [this](auto& arg) -> void {
                auto name = makeOSLUString(arg.getName());
                if (name.empty()) {
                    return;
                }

                d_arguments_by_name.insert({ name, &arg });
            });
    }
}

void
SymbolScope::add(const Symbol *s) {
    auto st       = s->symtype();
    const auto& t = s->typespec();
    auto name     = s->name();

    switch (s->symtype()) {
        case SymTypeGlobal: {
            assert(d_shader_globals);

            auto it = getShaderGlobalsIndex().find(name);
            assert(it != getShaderGlobalsIndex().end());

            auto address = d_builder.CreateStructGEP(nullptr, d_shader_globals, it->second);

            if (s->everwritten() || d_type_scope.isPassedByReference(t)) {
                address->setName(makeLLVMStringRef(name));
                addReference(s, address);

                if (t.is_closure()) {
                    if (name == "Ci") {
                        llosl_closure_Ci_annotation(address);
                    }
                    else if (s->everwritten()) {
                        llosl_closure_output_annotation(address);
                    }
                    else {
                        llosl_closure_storage_annotation(address);
                    }
                }
            }
            else {
                addValue(s, d_builder.CreateLoad(address, makeLLVMStringRef(name)));
            }
        } break;
        case SymTypeParam:
        case SymTypeOutputParam:  {
            auto it = d_arguments_by_name.find(name);
            assert(it != d_arguments_by_name.end());

            if (st == SymTypeOutputParam || d_type_scope.isPassedByReference(t)) {
                auto address = it->second;
                addReference(s, address);

                if (t.is_closure()) {
                    if (st == SymTypeOutputParam) {
                        llosl_closure_output_annotation(address);
                    }
                    else {
                        llosl_closure_storage_annotation(address);
                    }
                }
            }
            else {
                addValue(s, it->second);
            }
        } break;
        case SymTypeLocal:
        case SymTypeTemp: {
            auto value = d_type_scope.getConstant(t, s->data()).first;
            assert(value);

            auto address = allocate(s);

            if (t.is_closure()) {
                llosl_closure_storage_annotation(address);
            }

            d_builder.CreateStore(value, address);
        } break;
        case SymTypeConst: {
            auto value = d_type_scope.getConstant(t, s->data()).first;
            assert(value);

            if (d_type_scope.isPassedByReference(t)) {
                auto address = allocate(s);
                d_builder.CreateStore(value, address);
            }
            else {
                addValue(s, value);
            }
        } break;
        default:
            break;
    }
}

llvm::Value *
SymbolScope::getReference(const Symbol *s) {
    auto it = d_references.find(s);
    assert(it != d_references.end());

    return it->second;
}

llvm::Value *
SymbolScope::getValue(const Symbol *s) {
    auto it = d_values.find(s);
    assert(it != d_values.end());

    return it->second;
}

llvm::Value *
SymbolScope::getValueOrDereference(const Symbol *s) {
    auto it = d_values.find(s);
    if (it != d_values.end()) {
        return it->second;
    }

    auto jt = d_references.find(s);
    assert(jt != d_references.end());

    return d_builder.CreateLoad(jt->second);
}

llvm::Value *
SymbolScope::getValueForArgument(const Symbol *s) {
    return d_type_scope.isPassedByReference(s->typespec())
        ? getReference(s)
        : getValueOrDereference(s);
}

void
SymbolScope::addReference(const Symbol *s, llvm::Value *reference) {
    assert(d_references.count(s) == 0);
    assert(d_values.count(s) == 0);
    assert(reference->getType()->isPointerTy());

    d_references[s] = reference;
}

void
SymbolScope::addValue(const Symbol *s, llvm::Value *value) {
    assert(d_references.count(s) == 0);
    assert(d_values.count(s) == 0);

    d_values[s] = value;
}

llvm::Value *
SymbolScope::allocate(const Symbol *s) {
    const auto& t = s->typespec();
    auto name     = s->name();

    auto address = d_builder.CreateAlloca(d_type_scope.get(t), nullptr, makeLLVMStringRef(name));
    addReference(s, address);

    return address;
}

} // End namespace llosl