//-*-C++-*-
#ifndef LLOSL_IR_BXDFPASS_H
#define LLOSL_IR_BXDFPASS_H

#include <llvm/Pass.h>

#include <memory>
#include <stack>
#include <variant>

namespace llvm {
class raw_ostream;
} // End namespace llvm

namespace llosl {

class BXDFPass;

class BXDF {
public:

    struct Void;
    struct Component;
    struct WeightedComponent;
    struct Add;
    struct MulColor;
    struct MulFloat;

    using Node = std::variant<Void, Component, WeightedComponent, Add, MulColor, MulFloat>;
    using NodeRef = std::shared_ptr<Node>;

    struct Void {
    };

    struct Component {
        Component(unsigned id, unsigned address)
        : id(id)
        , address(address) {
        }

        unsigned id;
        unsigned address;
    };

    struct WeightedComponent {
        WeightedComponent(unsigned id, unsigned address, unsigned weight_address)
        : id(id)
        , address(address)
        , weight_address(weight_address) {
        }

        unsigned id;
        unsigned address;
        unsigned weight_address;
    };

    struct Add {
        Add(NodeRef lhs, NodeRef rhs)
        : lhs(lhs)
        , rhs(rhs) {
        }

        NodeRef lhs, rhs;
    };

    struct MulColor {
        MulColor(NodeRef lhs, unsigned rhs_address)
        : lhs(lhs)
        , rhs_address(rhs_address) {
        }

        NodeRef lhs;
        unsigned rhs_address;
    };

    struct MulFloat {
        MulFloat(NodeRef lhs, unsigned rhs_address)
        : lhs(lhs)
        , rhs_address(rhs_address) {
        }

        NodeRef lhs;
        unsigned rhs_address;
    };

    //
    template<typename Visitor>
    static void visit(NodeRef, Visitor&);

    template<typename Visitor, typename BackVisitor>
    static void visit(NodeRef, Visitor&, BackVisitor&);

    //
    static void print(llvm::raw_ostream&, NodeRef);

    //
    using Encoding = std::basic_string<uint8_t>;

    static Encoding encode(NodeRef);
    static NodeRef  decode(Encoding);

    NodeRef ast;
    unsigned heap_size = 0;
};

template<typename Visitor>
void
BXDF::visit(NodeRef node, Visitor& v) {
    std::stack<NodeRef> stack;

    stack.push(node);

    while (!stack.empty()) {
        auto ref = stack.top();
        stack.pop();

        std::visit([&v, &stack, ref](const auto& node) -> void {
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

template<typename Visitor, typename BackVisitor>
void
BXDF::visit(NodeRef node, Visitor& v, BackVisitor& bv) {
    struct Frame {
        NodeRef node;
        bool back = false;
    };

    std::stack<Frame> stack;

    stack.push({ node, false });

    while (!stack.empty()) {
        auto ref = stack.top().node;
        auto back = stack.top().back;
        stack.pop();

        if (!back) {
            stack.push({ ref, true });

            std::visit([&v, &stack, ref](const auto& node) -> void {
                v(ref, node);

                using T = std::decay_t<decltype(node)>;

                if constexpr (std::is_same_v<T, Add>) {
                    stack.push({ node.rhs, false });
                    stack.push({ node.lhs, false });
                }
                else if constexpr (std::is_same_v<T, MulColor>) {
                    stack.push({ node.lhs, false });
                }
                else if constexpr (std::is_same_v<T, MulFloat>) {
                    stack.push({ node.lhs, false });
                }
            },
            *ref);
        }
        else {
            std::visit([&bv, &stack, ref](const auto& node) -> void {
                bv(ref, node);
            },
            *ref);
        }
    }
}

class BXDFInfo {
public:

    unsigned getPathCount() const { return d_bxdfs.size(); }

    const BXDF& getBXDFForPath(unsigned path_id) const { return d_bxdfs[path_id]; }

    unsigned getMaxHeapSize() const { return d_max_heap_size; }

private:

    BXDFInfo(unsigned);

    void addBXDFForPath(unsigned, BXDF::NodeRef, unsigned);

    std::vector<BXDF> d_bxdfs;
    unsigned d_max_heap_size = 0;

    friend class BXDFPass;
};

class BXDFPass : public llvm::FunctionPass {
public:
    static char ID;

    BXDFPass();

    std::shared_ptr<const BXDFInfo> getBXDFInfo() const { return d_bxdf_info; }

    bool runOnFunction(llvm::Function &F) override;

    void getAnalysisUsage(llvm::AnalysisUsage &AU) const override;

private:

    std::shared_ptr<const BXDFInfo> d_bxdf_info;
};

llvm::FunctionPass *createBXDF();

} // End namespace llosl

#endif
