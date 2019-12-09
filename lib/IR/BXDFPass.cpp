#include <llosl/IR/Block.h>
#include <llosl/IR/BXDFPass.h>
#include <llosl/IR/ClosureFunction.h>
#include <llosl/IR/ClosureIRPass.h>
#include <llosl/IR/PathInfoPass.h>

#include <llvm/ADT/DenseMap.h>
#include <llvm/IR/Value.h>

#include <iostream>
#include <stack>
#include <type_traits>

namespace llosl {

// BXDFInfo:
BXDFInfo::BXDFInfo(unsigned path_count)
: d_bxdfs(path_count) {
}

void
BXDFInfo::addBXDFForPath(unsigned path_id, BXDFAST::NodeRef ast, unsigned heap_size) {
    d_bxdfs[path_id] = { ast, heap_size };
    d_max_heap_size = std::max(d_max_heap_size, heap_size);
}

std::unique_ptr<BXDFInfo>
BXDFInfo::Create(const ClosureFunction& function, const PathInfo& path_info) {
    std::unique_ptr<BXDFInfo> bxdf_info(new BXDFInfo(path_info.getPathCount()));

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
        std::vector<BXDFAST::NodeRef> storage;
        llvm::DenseMap<const Value *, BXDFAST::NodeRef> values;
        unsigned heap_size = 0;
    };

    std::stack<Frame> stack;

    stack.push(Frame(function));

    while (!stack.empty()) {
        auto frame = std::move(stack.top());
        stack.pop();

        auto block = frame.block;

        std::for_each(
            block->insts_begin(), block->insts_end(),
            [&bxdf_info, &frame](const auto& inst) -> void {
                switch(inst.getKind()) {
                case Value::ValueKind::Reference:
                case Value::ValueKind::Null: {
                    frame.values[&inst] = std::make_shared<BXDFAST::Node>(BXDFAST::Void());
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
                    frame.values[&inst] = std::make_shared<BXDFAST::Node>(
                        BXDFAST::Component(component_instruction->getClosureID(),
                                      address));
                } break;
                case Value::ValueKind::AllocateWeightedComponent: {
                    auto component_instruction = llvm::cast<AllocateWeightedComponent>(&inst);
                    auto component_address = frame.allocate(component_instruction->getClosureSize());
                    frame.values[&inst] = std::make_shared<BXDFAST::Node>(
                        BXDFAST::WeightedComponent(component_instruction->getClosureID(),
                                              component_address, 0));
                } break;
                case Value::ValueKind::AddClosureClosure: {
                    auto add_instruction = llvm::cast<AddClosureClosure>(&inst);
                    frame.values[&inst] = std::make_shared<BXDFAST::Node>(
                        BXDFAST::Add(frame.storage[add_instruction->getLHS()], frame.storage[add_instruction->getRHS()]));
                } break;
                case Value::ValueKind::MulClosureColor: {
                    auto mul_instruction = llvm::cast<MulClosureColor>(&inst);
                    auto address = frame.allocate(12);
                    frame.values[&inst] = std::make_shared<BXDFAST::Node>(
                        BXDFAST::MulColor(frame.storage[mul_instruction->getLHS()], address));
                } break;
                case Value::ValueKind::MulClosureFloat: {
                    auto mul_instruction = llvm::cast<MulClosureFloat>(&inst);
                    auto address = frame.allocate(4);
                    frame.values[&inst] = std::make_shared<BXDFAST::Node>(
                        BXDFAST::MulFloat(frame.storage[mul_instruction->getLHS()], address));
                } break;
                case Value::ValueKind::Cast: {
                    auto cast_instruction = llvm::cast<Cast>(&inst);
                    frame.values[&inst] = frame.values[cast_instruction->getOperand()];
                } break;
                case Value::ValueKind::Return: {
                    // TODO: storage[0] happens to always be 'Ci', but all outputs should be
                    // processed here, too:
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

                stack.push(Frame(frame, path_info, succ));
            });
    }

    return bxdf_info;
}

} // End namespace llosl