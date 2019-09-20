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

namespace llosl {

void
BXDF::print(llvm::raw_ostream& s, BXDF::NodeRef node) {
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

BXDF::Encoding
BXDF::encode(BXDF::NodeRef node) {
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

BXDF::NodeRef
BXDF::decode(BXDF::Encoding encoding) {
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

// BXDFInfo:
BXDFInfo::BXDFInfo(unsigned path_count)
: d_bxdfs(path_count) {
}

void
BXDFInfo::addBXDFForPath(unsigned path_id, BXDF::NodeRef ast, unsigned heap_size) {
    d_bxdfs[path_id] = { ast, heap_size };
    d_max_heap_size = std::max(d_max_heap_size, heap_size);
}

} // End namespace llosl

namespace llvm {

void initializeBXDFPassPass(PassRegistry&);

} // End namespace llvm

namespace llosl {

// BXDFPass:
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
        std::vector<BXDF::NodeRef> storage;
        llvm::DenseMap<const Value *, BXDF::NodeRef> values;
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
                    frame.values[&inst] = std::make_shared<BXDF::Node>(BXDF::Void());
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
                    frame.values[&inst] = std::make_shared<BXDF::Node>(
                        BXDF::Component(component_instruction->getClosureID(),
                                      address));
                } break;
                case Value::ValueKind::AllocateWeightedComponent: {
                    auto component_instruction = llvm::cast<AllocateWeightedComponent>(&inst);
                    auto component_address = frame.allocate(component_instruction->getClosureSize());
                    frame.values[&inst] = std::make_shared<BXDF::Node>(
                        BXDF::WeightedComponent(component_instruction->getClosureID(),
                                              component_address, 0));
                } break;
                case Value::ValueKind::AddClosureClosure: {
                    auto add_instruction = llvm::cast<AddClosureClosure>(&inst);
                    frame.values[&inst] = std::make_shared<BXDF::Node>(
                        BXDF::Add(frame.values[add_instruction->getLHS()], frame.values[add_instruction->getRHS()]));
                } break;
                case Value::ValueKind::MulClosureColor: {
                    auto mul_instruction = llvm::cast<MulClosureColor>(&inst);
                    auto address = frame.allocate(12);
                    frame.values[&inst] = std::make_shared<BXDF::Node>(
                        BXDF::MulColor(frame.values[mul_instruction->getLHS()], address));
                } break;
                case Value::ValueKind::MulClosureFloat: {
                    auto mul_instruction = llvm::cast<MulClosureFloat>(&inst);
                    auto address = frame.allocate(4);
                    frame.values[&inst] = std::make_shared<BXDF::Node>(
                        BXDF::MulFloat(frame.values[mul_instruction->getLHS()], address));
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
