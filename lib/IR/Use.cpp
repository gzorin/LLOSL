#include <llosl/IR/Use.h>
#include <llosl/IR/Value.h>

namespace llosl {

Use::Use() {
}

Use::Use(Value *value) {
    set(value);
}

Use::~Use() {
    set(nullptr);
}

void
Use::set(Value *value) {
    if (d_value == value) {
        return;
    }

    if (d_value) {
        d_value->d_uses.remove(*this);
    }

    d_value = value;

    if (d_value) {
        d_value->d_uses.push_back(*this);
    }
}

} // End namespace llosl
