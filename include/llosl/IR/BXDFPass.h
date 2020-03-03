//-*-C++-*-
#ifndef LLOSL_IR_BXDFPASS_H
#define LLOSL_IR_BXDFPASS_H

#include <llosl/IR/BXDFAST.h>

#include <memory>

namespace llosl {

class ClosureFunction;
class PathInfo;

class BXDFInfo {
public:
    static std::unique_ptr<BXDFInfo> Create(const ClosureFunction &, const PathInfo &);

    unsigned getPathCount() const { return d_bxdfs.size(); }

    const BXDFAST &getBXDFForPath(unsigned path_id) const { return d_bxdfs[path_id]; }

    unsigned getMaxHeapSize() const { return d_max_heap_size; }

private:
    BXDFInfo(unsigned);

    void addBXDFForPath(unsigned, BXDFAST::NodeRef, unsigned);

    std::vector<BXDFAST> d_bxdfs;
    unsigned             d_max_heap_size = 0;

    friend class BXDFPass;
};

} // End namespace llosl

#endif
