//-*-C++-*-
#ifndef LLOSL_TYPESCOPE_H
#define LLOSL_TYPESCOPE_H

#include <osl_pvt.h>

namespace llosl {

class LLOSLContextImpl;

class TypeScope {
public:
    TypeScope(LLOSLContextImpl &);

    llvm::Type *      get(const OSL::TypeDesc &t) { return d_context.getLLVMType(t, false); }
    llvm::Type *      get(const OSL::pvt::TypeSpec &);
    llvm::StructType *get(OSL::ustring);

    bool isPassedByReference(const OSL::TypeDesc &t) const {
        return d_context.isTypePassedByReference(t);
    }
    bool isPassedByReference(const OSL::pvt::TypeSpec &) const;

    llvm::Type *getForArgument(const OSL::TypeDesc &t) {
        return d_context.getLLVMTypeForArgument(t, false);
    }
    llvm::Type *getForArgument(const OSL::pvt::TypeSpec &);

    llvm::Constant *                          getDefaultConstant(const OSL::pvt::TypeSpec &);
    std::pair<llvm::Constant *, const void *> getConstant(const OSL::pvt::TypeSpec &,
                                                          const void * = nullptr);

private:
    LLOSLContextImpl &d_context;

    std::unordered_map<int, llvm::StructType *> d_structs;
    std::map<OSL::ustring, int>                 d_structs_by_name;
};

} // End namespace llosl

#endif
