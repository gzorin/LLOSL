//-*-C++-*-
#ifndef LLOSL_IR_USE_H
#define LLOSL_IR_USE_H

#include <llvm/ADT/ilist_node.h>

namespace llosl {

class Value;

class Use : public llvm::ilist_node<Use> {
public:
    Use();
    Use(Value *);
    ~Use();

    void set(Value *);

    Value *      get() { return d_value; }
    const Value *get() const { return d_value; }

    operator Value *() { return d_value; }
    operator const Value *() const { return d_value; }

    Value *      operator->() { return d_value; }
    const Value *operator->() const { return d_value; }

private:
    Value *d_value = nullptr;
};

} // End namespace llosl

#endif
