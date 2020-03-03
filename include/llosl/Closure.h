//-*-C++-*-
#ifndef LLOSL_CLOSURE_H
#define LLOSL_CLOSURE_H

#include <llvm/ADT/ArrayRef.h>
#include <llvm/ADT/StringRef.h>

#include <OSL/genclosure.h>
#include <OSL/oslconfig.h>

#include <memory>
#include <string>
#include <vector>

namespace llvm {
class Function;
class Module;
class StructType;
} // End namespace llvm

namespace llosl {

class LLOSLContext;
class LLOSLContextImpl;

class Closure {
public:
    static std::unique_ptr<Closure> create(LLOSLContext &, llvm::StringRef, unsigned,
                                           llvm::ArrayRef<OSL::ClosureParam>,
                                           llvm::Module * = nullptr);

    llvm::StringRef name() const { return d_name; }
    unsigned        id() const { return d_id; }

    using param_list_type      = std::vector<OSL::TypeDesc>;
    using param_iterator       = param_list_type::iterator;
    using const_param_iterator = param_list_type::const_iterator;

    param_iterator       param_begin() { return d_param_list.begin(); }
    const_param_iterator param_begin() const { return d_param_list.begin(); }

    param_iterator       param_end() { return d_param_list.end(); }
    const_param_iterator param_end() const { return d_param_list.end(); }

    using param_reverse_iterator       = param_list_type::reverse_iterator;
    using const_param_reverse_iterator = param_list_type::const_reverse_iterator;

    param_reverse_iterator       param_rbegin() { return d_param_list.rbegin(); }
    const_param_reverse_iterator param_rbegin() const { return d_param_list.rbegin(); }

    param_reverse_iterator       param_rend() { return d_param_list.rend(); }
    const_param_reverse_iterator param_rend() const { return d_param_list.rend(); }

    unsigned param_count() const { return d_param_list.size(); }

    llvm::StructType *params_type() const { return d_params_type; }

    llvm::Function *function() const { return d_function; }

private:
    static std::unique_ptr<Closure> create(LLOSLContextImpl &, llvm::StringRef, unsigned,
                                           llvm::ArrayRef<OSL::ClosureParam>,
                                           llvm::Module * = nullptr);

    Closure();

    std::string d_name;
    unsigned    d_id = 0;

    param_list_type   d_param_list;
    llvm::StructType *d_params_type = nullptr;
    llvm::Function *  d_function    = nullptr;

    friend class LLOSLContextImpl;
    friend std::unique_ptr<Closure> std::make_unique<Closure>();
};

} // End namespace llosl

#endif