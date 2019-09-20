#include <llosl/IR/Block.h>
#include <llosl/IR/BXDFPass.h>
#include <llosl/IR/ClosureFunction.h>
#include <llosl/IR/ClosureIRPass.h>
#include <llosl/IR/PathInfoPass.h>

#include <llvm/ADT/DenseMap.h>
#include <llvm/IR/Value.h>
#include <llvm/Support/raw_ostream.h>

#include <iostream>
#include <stack>
#include <type_traits>
#include <unordered_map>

namespace llvm {

void initializeBXDFPassPass(PassRegistry&);

} // End namespace llvm

namespace llosl {

void
print(llvm::raw_ostream& s, BXDFNodeRef node) {
    struct Frame {
        BXDFNodeRef node;
        unsigned indent = 0;
        bool back;
    };

    std::stack<Frame> stack;

    stack.push({ node, 0, false });

    while (!stack.empty()) {
        auto [ node, indent, back ] = stack.top();
        stack.pop();

        if (!back) {
            stack.push({ node, indent, true });

            if (!node) {
                s << "X ";
                continue;
            }

            std::visit([&s, &stack](auto && node) -> void {
                unsigned indent = 0;

                using T = std::decay_t<decltype(node)>;

                if constexpr (std::is_same_v<T, BXDFVoid>) {
                    s << "? ";
                }
                else if constexpr (std::is_same_v<T, BXDFComponent>) {
                    s << "(" << node.id << " [" << node.address << "]) ";
                }
                else if constexpr (std::is_same_v<T, BXDFWeightedComponent>) {
                    s << "(" << node.id << " [" << node.address << "]) ";
                }
                else if constexpr (std::is_same_v<T, BXDFAdd>) {
                    s << "(+ ";
                    stack.push({ node.rhs, indent, false });
                    stack.push({ node.lhs, indent, false });
                }
                else if constexpr (std::is_same_v<T, BXDFMulColor>) {
                    s << "(*c ";
                    stack.push({ node.lhs, indent, false });
                }
                else if constexpr (std::is_same_v<T, BXDFMulFloat>) {
                    s << "(*f ";
                    stack.push({ node.lhs, indent, false });
                }
            }, *node);
        }
        else {
            if (!node) {
                continue;
            }

            std::visit([&s, &stack](auto && node) -> void {
                using T = std::decay_t<decltype(node)>;

                if constexpr (std::is_same_v<T, BXDFAdd>) {
                    s << ")";
                }
                else if constexpr (std::is_same_v<T, BXDFMulColor>) {
                    s << "[" << node.rhs_address <<  "])";
                }
                else if constexpr (std::is_same_v<T, BXDFMulFloat>) {
                    s << "[" << node.rhs_address <<  "])";
                }
            }, *node);
        }
    }
}

template<typename Visitor>
void
visitBXDF(BXDFNodeRef node, Visitor& v) {
    std::stack<BXDFNodeRef> stack;

    stack.push(node);

    while (!stack.empty()) {
        auto ref = stack.top();
        stack.pop();

        std::visit([&v, &stack, ref](const auto& node) -> void {
            v(ref, node);

            using T = std::decay_t<decltype(node)>;

            if constexpr (std::is_same_v<T, BXDFAdd>) {
                stack.push(node.rhs);
                stack.push(node.lhs);
            }
            else if constexpr (std::is_same_v<T, BXDFMulColor>) {
                stack.push(node.lhs);
            }
            else if constexpr (std::is_same_v<T, BXDFMulFloat>) {
                stack.push(node.lhs);
            }
        },
        *ref);
    }
}

enum class Opcode : uint8_t {
    Void              = 0 << 5,
    Component         = 1 << 5,
    WeightedComponent = 2 << 5,
    Add               = 3 << 5,
    MulColor          = 4 << 5,
    MulFloat          = 5 << 5
};

BXDFEncoding
encodeBXDF(BXDFNodeRef node) {
    if (!node) {
        return BXDFEncoding();
    }

    // Allocator
    struct Allocate {
        std::size_t size = 0;
        std::unordered_map<BXDFNodeRef, std::size_t> position;

        void operator()(BXDFNodeRef, const BXDFVoid& node) {
        }

        void operator()(BXDFNodeRef ref, const BXDFComponent& node) {
            position[ref] = size;
            size += 2;
        }

        void operator()(BXDFNodeRef ref, const BXDFWeightedComponent& node) {
            position[ref] = size;
            size += 2;
        }

        void operator()(BXDFNodeRef ref, const BXDFAdd& node) {
            position[ref] = size;
            size += 1;
        }

        void operator()(BXDFNodeRef ref, const BXDFMulColor& node) {
            position[ref] = size;
            size += 1;
        }

        void operator()(BXDFNodeRef ref, const BXDFMulFloat& node) {
            position[ref] = size;
            size += 1;
        }
    };

    Allocate allocate;
    visitBXDF(node, allocate);
    BXDFEncoding encoding(allocate.size, 0);

    struct Encoder {
        BXDFEncoding::iterator it;
        std::unordered_map<BXDFNodeRef, std::size_t> position;

        void operator()(BXDFNodeRef, const BXDFVoid& node) {
        }

        void operator()(BXDFNodeRef, const BXDFComponent& node) {
            *it++ = (uint8_t)Opcode::Component | ((uint8_t)node.id & 0x1F);
            *it++ = (uint8_t)node.address;
        }

        void operator()(BXDFNodeRef, const BXDFWeightedComponent& node) {
            *it++ = (uint8_t)Opcode::WeightedComponent | ((uint8_t)node.id & 0x1F);
            *it++ = (uint8_t)node.address;
        }

        void operator()(BXDFNodeRef, const BXDFAdd& node) {
            auto rhs_position = position[node.rhs];
            *it++ = (uint8_t)Opcode::Add | ((uint8_t)rhs_position & 0x1F);
        }

        void operator()(BXDFNodeRef, const BXDFMulColor& node) {
            *it++ = (uint8_t)Opcode::MulColor | ((uint8_t)(node.rhs_address >> 2) & 0x1F);
        }

        void operator()(BXDFNodeRef, const BXDFMulFloat& node) {
            *it++ = (uint8_t)Opcode::MulFloat | ((uint8_t)(node.rhs_address >> 2) & 0x1F);
        }
    };

    Encoder encode = { encoding.begin(), std::move(allocate.position) };
    visitBXDF(node, encode);

    return encoding;
}

BXDFNodeRef
decodeDetail(BXDFEncoding::const_iterator it_begin, BXDFEncoding::const_iterator it) {
    auto code = *it++;
    Opcode opcode = (Opcode)(code & 0xE0);
    auto operand = (unsigned)(code & 0x1F);

    switch (opcode) {
    case Opcode::Void: {
        return std::shared_ptr<BXDFNode>();
    } break;
    case Opcode::Component: {
        return std::make_shared<BXDFNode>(BXDFComponent(operand, *it++));
    } break;
    case Opcode::WeightedComponent: {
        return std::make_shared<BXDFNode>(BXDFWeightedComponent(operand, *it++, 0));
    } break;
    case Opcode::Add: {
        return std::make_shared<BXDFNode>(BXDFAdd(decodeDetail(it_begin, it), decodeDetail(it_begin, it_begin + operand)));
    } break;
    case Opcode::MulColor: {
        return std::make_shared<BXDFNode>(BXDFMulColor(decodeDetail(it_begin, it), operand << 2));
    } break;
    case Opcode::MulFloat: {
        return std::make_shared<BXDFNode>(BXDFMulFloat(decodeDetail(it_begin, it), operand << 2));
    } break;
    }
}

BXDFNodeRef
decodeBXDF(BXDFEncoding encoding) {
    return decodeDetail(encoding.begin(), encoding.begin());
}

BXDFInfo::BXDFInfo(unsigned path_count)
: d_bxdfs(path_count) {
}

void
BXDFInfo::addBXDFForPath(unsigned path_id, BXDFNodeRef ast, unsigned heap_size) {
    d_bxdfs[path_id] = { ast, heap_size };
    d_max_heap_size = std::max(d_max_heap_size, heap_size);
}

BXDFPass::BXDFPass() : FunctionPass(ID) {
    llvm::initializeBXDFPassPass(*llvm::PassRegistry::getPassRegistry());
}

char BXDFPass::ID = 0;

llvm::FunctionPass *createBXDFPass() {
    return new BXDFPass();
}

bool BXDFPass::runOnFunction(llvm::Function &F) {
    auto function = getAnalysis<ClosureIRPass>().getIR();
    auto path_info = getAnalysis<PathInfoPass>().getPathInfo();

    std::unique_ptr<BXDFInfo> bxdf_info(new BXDFInfo(path_info->getPathCount()));

    struct Frame {
        Frame(const ClosureFunction& function)
        : block(&*function.blocks_begin())
        , storage(function.getClosureStorageCount()) {
        }

        Frame(const Frame& prev, const PathInfo& path_info, const Block *block)
        : block(block)
        , path_id(prev.path_id + *path_info.getEdgeIDForBlocks(prev.block, block))
        , storage(prev.storage)
        , values(prev.values)
        , heap_size(prev.heap_size) {
        }

        unsigned allocate(unsigned size) {
            return std::exchange(heap_size, heap_size + size);
        }

        const Block *block = nullptr;

        unsigned path_id = 0;
        std::vector<BXDFNodeRef> storage;
        llvm::DenseMap<const Value *, BXDFNodeRef> values;
        unsigned heap_size = 0;
    };

    std::stack<Frame> stack;

    stack.push(Frame(*function));

    while (!stack.empty()) {
        auto frame = std::move(stack.top());
        stack.pop();

        auto block = frame.block;

        std::for_each(
            block->insts_begin(), block->insts_end(),
            [path_info, &bxdf_info, &frame](const auto& inst) -> void {
                switch(inst.getKind()) {
                case Value::ValueKind::Alloca: {
                    frame.values[&inst] = std::make_shared<BXDFNode>(BXDFVoid());
                } break;
                case Value::ValueKind::Load: {
                    auto load_instruction = llvm::cast<Load>(&inst);
                    frame.values[&inst] = frame.storage[load_instruction->getLocation()];
                } break;
                case Value::ValueKind::Store: {
                    auto store_instruction = llvm::cast<Store>(&inst);
                    frame.storage[store_instruction->getLocation()] = frame.values[store_instruction->getOperand()];
                } break;
                case Value::ValueKind::AllocateComponent: {
                    auto component_instruction = llvm::cast<AllocateComponent>(&inst);
                    auto address = frame.allocate(component_instruction->getClosureSize());
                    frame.values[&inst] = std::make_shared<BXDFNode>(
                        BXDFComponent(component_instruction->getClosureID(),
                                      address));
                } break;
                case Value::ValueKind::AllocateWeightedComponent: {
                    auto component_instruction = llvm::cast<AllocateWeightedComponent>(&inst);
                    auto component_address = frame.allocate(component_instruction->getClosureSize());
                    frame.values[&inst] = std::make_shared<BXDFNode>(
                        BXDFWeightedComponent(component_instruction->getClosureID(),
                                              component_address, 0));
                } break;
                case Value::ValueKind::AddClosureClosure: {
                    auto add_instruction = llvm::cast<AddClosureClosure>(&inst);
                    frame.values[&inst] = std::make_shared<BXDFNode>(
                        BXDFAdd(frame.values[add_instruction->getLHS()], frame.values[add_instruction->getRHS()]));
                } break;
                case Value::ValueKind::MulClosureColor: {
                    auto mul_instruction = llvm::cast<MulClosureColor>(&inst);
                    auto address = frame.allocate(12);
                    frame.values[&inst] = std::make_shared<BXDFNode>(
                        BXDFMulColor(frame.values[mul_instruction->getLHS()], address));
                } break;
                case Value::ValueKind::MulClosureFloat: {
                    auto mul_instruction = llvm::cast<MulClosureFloat>(&inst);
                    auto address = frame.allocate(4);
                    frame.values[&inst] = std::make_shared<BXDFNode>(
                        BXDFMulFloat(frame.values[mul_instruction->getLHS()], address));
                } break;
                case Value::ValueKind::Cast: {
                    auto cast_instruction = llvm::cast<Cast>(&inst);
                    frame.values[&inst] = frame.values[cast_instruction->getOperand()];
                } break;
                case Value::ValueKind::Return: {
                    bxdf_info->addBXDFForPath(frame.path_id, frame.storage[0], frame.heap_size);
                } break;
                default:
                    break;
                }
            });

        std::for_each(
            frame.block->succs_begin(), frame.block->succs_end(),
            [path_info, &stack, &frame](const auto& tmp) -> void {
                auto succ = tmp.first;

                stack.push(Frame(frame, *path_info, succ));
            });
    }

    d_bxdf_info = std::move(bxdf_info);

    for (unsigned i = 0, n = d_bxdf_info->getPathCount(); i < n; ++i) {
        llvm::errs() << i << ":\n";

        const auto& bxdf = d_bxdf_info->getBXDFForPath(i);
        llvm::errs() << "\t";
        llosl::print(llvm::errs(), bxdf.ast);
        llvm::errs() << "\n";

        auto encoding = encodeBXDF(bxdf.ast);

        llvm::errs() << "\t";
        for (auto byte : encoding) {
            llvm::errs() << (uint32_t)byte << ";";
        }
        llvm::errs() << "\n";

        auto laundered = decodeBXDF(encoding);
        llvm::errs() << "\t";
        llosl::print(llvm::errs(), laundered);
        llvm::errs() << "\n";
    }

    return true;
}

void BXDFPass::getAnalysisUsage(llvm::AnalysisUsage &AU) const {
    AU.setPreservesAll();
    AU.addRequired<ClosureIRPass>();
    AU.addRequired<PathInfoPass>();
}

} // End namespace llosl

using llosl::BXDFPass;
using namespace llvm;

INITIALIZE_PASS_BEGIN(BXDFPass, "llosl-bxdf",
                      "BXDF", false, false)
INITIALIZE_PASS_DEPENDENCY(AAResultsWrapperPass)
INITIALIZE_PASS_END(BXDFPass, "llosl-bxdf",
                    "BXDF", false, false)
