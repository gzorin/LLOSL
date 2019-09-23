//-*-C++-*-
#ifndef LLOSL_UBERBXDF_H
#define LLOSL_UBERBXDF_H

#include <llosl/BXDF.h>

#include <llvm/ADT/ilist_node.h>
#include <llvm/Transforms/Utils/ValueMapper.h>

#include <map>

namespace llvm {
class BasicBlock;
class Function;
class Module;
class SwitchInst;
} // End namespace llvm

namespace llosl {

class LLOSLContextImpl;

class UberBXDF : public llvm::ilist_node<UberBXDF> {
public:

    ~UberBXDF();

    UberBXDF() = delete;
    UberBXDF(const UberBXDF&) = delete;
    UberBXDF(UberBXDF&&) = delete;

    std::pair<uint16_t, bool> insertBXDF(const BXDF *);

    const llvm::Module *module() { return d_module.get(); }

    const llvm::Function *getFunction();

    std::size_t max_heap_size() const      { return d_max_heap_size; }

private:

    UberBXDF(LLOSLContextImpl&);

    struct Branch {
        const BXDF       *bxdf     = nullptr;
        uint16_t          index    = 0;
        llvm::Function   *function = nullptr;
        llvm::BasicBlock *block    = nullptr;
    };

    std::map<BXDF::Encoding, Branch, std::less<> > d_index;

    LLOSLContextImpl *d_context;
    std::unique_ptr<llvm::Module> d_module;
    llvm::ValueToValueMapTy d_bxdf_component_mapping;
    llvm::Function *d_function = nullptr;
    llvm::SwitchInst *d_switch_instruction = nullptr;
    std::size_t d_max_heap_size = 0;
    bool d_modified = true;

    friend class LLOSLContextImpl;
};

} // End namespace llosl

#endif
