//-*-C++-*-
#ifndef LLOSL_BXDFAST_H
#define LLOSL_BXDFAST_H

#include <stack>
#include <string>
#include <variant>

namespace llvm {
class raw_ostream;
} // End namespace llvm

namespace llosl {

struct BXDFAST {
    struct Void;
    struct Component;
    struct WeightedComponent;
    struct Add;
    struct MulColor;
    struct MulFloat;

    using Node    = std::variant<Void, Component, WeightedComponent, Add, MulColor, MulFloat>;
    using NodeRef = std::shared_ptr<Node>;

    struct Void {};

    struct Component {
        Component(unsigned id, unsigned address)
            : id(id)
            , address(address) {}

        unsigned id;
        unsigned address;
    };

    struct WeightedComponent {
        WeightedComponent(unsigned id, unsigned address, unsigned weight_address)
            : id(id)
            , address(address)
            , weight_address(weight_address) {}

        unsigned id;
        unsigned address;
        unsigned weight_address;
    };

    struct Add {
        Add(NodeRef lhs, NodeRef rhs)
            : lhs(lhs)
            , rhs(rhs) {}

        NodeRef lhs, rhs;
    };

    struct MulColor {
        MulColor(NodeRef lhs, unsigned rhs_address)
            : lhs(lhs)
            , rhs_address(rhs_address) {}

        NodeRef  lhs;
        unsigned rhs_address;
    };

    struct MulFloat {
        MulFloat(NodeRef lhs, unsigned rhs_address)
            : lhs(lhs)
            , rhs_address(rhs_address) {}

        NodeRef  lhs;
        unsigned rhs_address;
    };

    //
    template <typename Visitor> static void visit(NodeRef, Visitor &);

    template <typename Visitor, typename BackVisitor>
    static void visit(NodeRef, Visitor &, BackVisitor &);

    //
    static void print(llvm::raw_ostream &, NodeRef);

    //
    using Encoding = std::basic_string<uint8_t>;

    static Encoding encode(NodeRef);
    static NodeRef  decode(Encoding);

    NodeRef  ast;
    unsigned heap_size = 0;
};

template <typename Visitor>
void
BXDFAST::visit(NodeRef node, Visitor &v) {
    std::stack<NodeRef> stack;

    stack.push(node);

    while (!stack.empty()) {
        auto ref = stack.top();
        stack.pop();

        std::visit(
            [&v, &stack, ref](const auto &node) -> void {
                v(ref, node);

                using T = std::decay_t<decltype(node)>;

                if constexpr (std::is_same_v<T, Add>) {
                    stack.push(node.rhs);
                    stack.push(node.lhs);
                }
                else if constexpr (std::is_same_v<T, MulColor>) {
                    stack.push(node.lhs);
                }
                else if constexpr (std::is_same_v<T, MulFloat>) {
                    stack.push(node.lhs);
                }
            },
            *ref);
    }
}

template <typename Visitor, typename BackVisitor>
void
BXDFAST::visit(NodeRef node, Visitor &v, BackVisitor &bv) {
    struct Frame {
        NodeRef node;
        bool    back = false;
    };

    std::stack<Frame> stack;

    stack.push({node, false});

    while (!stack.empty()) {
        auto ref  = stack.top().node;
        auto back = stack.top().back;
        stack.pop();

        if (!back) {
            stack.push({ref, true});

            std::visit(
                [&v, &stack, ref](const auto &node) -> void {
                    v(ref, node);

                    using T = std::decay_t<decltype(node)>;

                    if constexpr (std::is_same_v<T, Add>) {
                        stack.push({node.rhs, false});
                        stack.push({node.lhs, false});
                    }
                    else if constexpr (std::is_same_v<T, MulColor>) {
                        stack.push({node.lhs, false});
                    }
                    else if constexpr (std::is_same_v<T, MulFloat>) {
                        stack.push({node.lhs, false});
                    }
                },
                *ref);
        }
        else {
            std::visit([&bv, &stack, ref](const auto &node) -> void { bv(ref, node); }, *ref);
        }
    }
}

} // End namespace llosl

#endif
