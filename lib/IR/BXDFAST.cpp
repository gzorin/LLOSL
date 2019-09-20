#include <llosl/IR/BXDFAST.h>

#include <llvm/Support/raw_ostream.h>

#include <unordered_map>

namespace llosl {

void
BXDFAST::print(llvm::raw_ostream& s, BXDFAST::NodeRef node) {
    if (!node) {
        s << "X";
        return;
    }

    struct Print {
        llvm::raw_ostream& s;

        void operator()(NodeRef, const Void&) {
            s << "?";
        }

        void operator()(NodeRef, const Component& node) {
            s << node.id << "[" << node.address << "]";
        }

        void operator()(NodeRef, const WeightedComponent& node) {
            s << node.id << "[" << node.address << "]*w";
        }

        void operator()(NodeRef, const Add& node) {
            s << "(+";
        }

        void operator()(NodeRef, const MulColor& node) {
            s << "(";
        }

        void operator()(NodeRef, const MulFloat& node) {
            s << "(";
        }
    };

    struct PrintBack {
        llvm::raw_ostream& s;

        void operator()(NodeRef, const Void&) {
        }

        void operator()(NodeRef, const Component& node) {
        }

        void operator()(NodeRef, const WeightedComponent& node) {
        }

        void operator()(NodeRef, const Add& node) {
            s << ")";
        }

        void operator()(NodeRef, const MulColor& node) {
            s << "*c[" << node.rhs_address << "])";
        }

        void operator()(NodeRef, const MulFloat& node) {
            s << "*f[" << node.rhs_address << "])";
        }
    };

    Print     v  = { s };
    PrintBack bv = { s };
    visit(node, v, bv);
}

namespace {

enum class Opcode : uint8_t {
    Void              = 0 << 5,
    Component         = 1 << 5,
    WeightedComponent = 2 << 5,
    Add               = 3 << 5,
    MulColor          = 4 << 5,
    MulFloat          = 5 << 5
};

uint8_t encode(Opcode opcode, unsigned operand) {
    return (uint8_t)opcode | ((uint8_t)operand & 0x1F);
}

std::pair<Opcode, unsigned> decode(uint8_t code) {
    return std::make_pair((Opcode)(code & 0xE0), (unsigned)(code & 0x1F));
}

} // End anonymous namespace

BXDFAST::Encoding
BXDFAST::encode(BXDFAST::NodeRef node) {
    if (!node) {
        return Encoding();
    }

    // Allocator
    struct Allocate {
        unsigned size = 0;
        std::unordered_map<NodeRef, unsigned> position;

        void operator()(NodeRef, const Void& node) {
        }

        void operator()(NodeRef ref, const Component& node) {
            position[ref] = size;
            size += 2;
        }

        void operator()(NodeRef ref, const WeightedComponent& node) {
            position[ref] = size;
            size += 2;
        }

        void operator()(NodeRef ref, const Add& node) {
            position[ref] = size;
            size += 1;
        }

        void operator()(NodeRef ref, const MulColor& node) {
            position[ref] = size;
            size += 1;
        }

        void operator()(NodeRef ref, const MulFloat& node) {
            position[ref] = size;
            size += 1;
        }
    };

    Allocate allocate;
    visit(node, allocate);
    Encoding encoding(allocate.size + 1, 0);

    struct Encoder {
        Encoding::iterator it;
        std::unordered_map<NodeRef, unsigned> position;

        void operator()(NodeRef, const Void& node) {
        }

        void operator()(NodeRef, const Component& node) {
            *it++ = llosl::encode(Opcode::Component, node.id);
            *it++ = (uint8_t)node.address;
        }

        void operator()(NodeRef, const WeightedComponent& node) {
            *it++ = llosl::encode(Opcode::WeightedComponent, node.id);
            *it++ = (uint8_t)node.address;
        }

        void operator()(NodeRef, const Add& node) {
            *it++ = llosl::encode(Opcode::Add, position[node.rhs]);
        }

        void operator()(NodeRef, const MulColor& node) {
            *it++  = llosl::encode(Opcode::MulColor, node.rhs_address >> 2);
        }

        void operator()(NodeRef, const MulFloat& node) {
            *it++  = llosl::encode(Opcode::MulFloat, node.rhs_address >> 2);
        }
    };

    auto it = encoding.begin();
    *it++ = 5;

    Encoder encode = { it, std::move(allocate.position) };
    visit(node, encode);

    return encoding;
}

BXDFAST::NodeRef
BXDFAST::decode(BXDFAST::Encoding encoding) {
    auto it_begin = encoding.begin() + 1;

    std::function<NodeRef(Encoding::const_iterator)> detail =
    [it_begin, &detail](Encoding::const_iterator it) -> NodeRef {
        auto [ opcode, operand ] = llosl::decode(*it++);

        switch (opcode) {
        case Opcode::Void: {
            return std::shared_ptr<Node>();
        } break;
        case Opcode::Component: {
            return std::make_shared<Node>(
                Component(operand, *it++));
        } break;
        case Opcode::WeightedComponent: {
            return std::make_shared<Node>(
                WeightedComponent(operand, *it++, 0));
        } break;
        case Opcode::Add: {
            return std::make_shared<Node>(
                Add(detail(it),
                    detail(it_begin + operand)));
        } break;
        case Opcode::MulColor: {
            return std::make_shared<Node>(
                MulColor(detail(it), operand << 2));
        } break;
        case Opcode::MulFloat: {
            return std::make_shared<Node>(
                MulFloat(detail(it), operand << 2));
        } break;
        }
    };

    return detail(it_begin);
}

} // End namespace llosl
