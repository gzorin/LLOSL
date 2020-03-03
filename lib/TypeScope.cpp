//-*-C++-*-

#include "TypeScope.h"
#include "LLOSLContextImpl.h"

#include <llvm/IR/Constants.h>

namespace llosl {

TypeScope::TypeScope(LLOSLContextImpl &context)
    : d_context(context) {}

llvm::Type *
TypeScope::get(const OSL::pvt::TypeSpec &t) {
    if (t.is_closure_based()) {
        auto type = d_context.getLLVMClosureType();

        if (t.is_closure_array()) {
            return llvm::ArrayType::get(type, t.arraylength());
        }

        return type;
    }

    if (t.is_structure_based()) {
        auto struct_id = t.structure();

        auto it = d_structs.find(struct_id);
        if (it == d_structs.end()) {
            auto                      struct_spec = t.structspec();
            std::vector<llvm::Type *> member_types;
            member_types.reserve(struct_spec->numfields());

            for (int i = 0, n = struct_spec->numfields(); i < n; ++i) {
                member_types.push_back(get(struct_spec->field(i).type));
            }

            auto struct_type = llvm::StructType::get(d_context.getLLContext(), member_types);
            it               = d_structs.insert({struct_id, struct_type}).first;
            d_structs_by_name.insert({struct_spec->name(), struct_id});
        }

        assert(it != d_structs.end());

        if (t.is_structure_array()) {
            return llvm::ArrayType::get(it->second, t.arraylength());
        }

        return it->second;
    }

    return d_context.getLLVMType(t.simpletype(), false);
}

llvm::StructType *
TypeScope::get(OSL::ustring name) {
    auto it = d_structs_by_name.find(name);
    if (it == d_structs_by_name.end()) {
        return nullptr;
    }

    auto jt = d_structs.find(it->second);
    if (jt == d_structs.end()) {
        return nullptr;
    }

    return jt->second;
}

bool
TypeScope::isPassedByReference(const OSL::pvt::TypeSpec &t) const {
    if (t.is_closure() || t.is_structure()) {
        return true;
    }

    if (t.is_array()) {
        return true;
    }

    return d_context.isTypePassedByReference(t.simpletype());
}

llvm::Type *
TypeScope::getForArgument(const OSL::pvt::TypeSpec &t) {
    if (t.is_closure() || t.is_structure()) {
        return llvm::PointerType::get(get(t), 0);
    }

    if (t.is_array()) {
        return llvm::PointerType::get(get(t.elementtype()), 0);
    }

    return d_context.getLLVMTypeForArgument(t.simpletype(), false);
}

llvm::Constant *
TypeScope::getDefaultConstant(const OSL::pvt::TypeSpec &t) {
    if (t.is_closure_array()) {
        std::vector<llvm::Constant *> elements(t.arraylength(),
                                               d_context.getLLVMClosureDefaultConstant());

        return llvm::ConstantArray::get(llvm::cast<llvm::ArrayType>(get(t)), elements);
    }

    if (t.is_closure()) {
        return d_context.getLLVMClosureDefaultConstant();
    }

    if (t.is_structure_array()) {
        auto               struct_spec = t.structspec();
        OSL::pvt::TypeSpec element_type(nullptr, t.structure());

        std::vector<llvm::Constant *> elements(t.arraylength(), getDefaultConstant(element_type));

        return llvm::ConstantArray::get(llvm::cast<llvm::ArrayType>(get(t)), elements);
    }

    if (t.is_structure()) {
        auto struct_spec = t.structspec();

        std::vector<llvm::Constant *> members(struct_spec->numfields(), nullptr);
        int                           i = 0;

        std::generate_n(members.begin(), struct_spec->numfields(),
                        [this, struct_spec, &i]() -> llvm::Constant * {
                            return getDefaultConstant(struct_spec->field(i++).type);
                        });

        return llvm::ConstantStruct::get(llvm::cast<llvm::StructType>(get(t)), members);
    }

    return d_context.getLLVMDefaultConstant(t.simpletype(), false);
}

std::pair<llvm::Constant *, const void *>
TypeScope::getConstant(const OSL::pvt::TypeSpec &t, const void *p) {
    if (!p) {
        return {getDefaultConstant(t), nullptr};
    }

    if (t.is_closure_array() || t.is_closure()) {
        assert(false);
        return {nullptr, p};
    }

    if (t.is_structure_array()) {
        auto               struct_spec = t.structspec();
        OSL::pvt::TypeSpec element_type(nullptr, t.structure());

        std::vector<llvm::Constant *> elements(t.arraylength(), nullptr);

        std::generate_n(elements.begin(), t.arraylength(),
                        [this, &p, &element_type]() -> llvm::Constant * {
                            auto [element, next_p] = getConstant(element_type, p);
                            p                      = next_p;
                            return element;
                        });

        return {llvm::ConstantArray::get(llvm::cast<llvm::ArrayType>(get(t)), elements), p};
    }

    if (t.is_structure()) {
        auto struct_spec = t.structspec();

        std::vector<llvm::Constant *> members(struct_spec->numfields(), nullptr);
        int                           i = 0;

        std::generate_n(members.begin(), struct_spec->numfields(),
                        [this, &p, struct_spec, &i]() -> llvm::Constant * {
                            auto [member, next_p] = getConstant(struct_spec->field(i++).type, p);
                            p                     = next_p;
                            return member;
                        });

        return {llvm::ConstantStruct::get(llvm::cast<llvm::StructType>(get(t)), members), p};
    }

    return d_context.getLLVMConstant(t.simpletype(), p, false);
}

} // End namespace llosl