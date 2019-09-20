#ifndef LLOSL_BXDFSCOPEIMPL_H
#define LLOSL_BXDFSCOPEIMPL_H

#include <llosl/IR/BXDFPass.h>

#include <unordered_map>

namespace llosl {

class ShaderGroup;

class BXDFScopeImpl {
public:

    BXDFScopeImpl();
    ~BXDFScopeImpl();

    void insertBXDFsFromShaderGroup(const ShaderGroup&);

private:

    std::unordered_map<BXDFAST::Encoding, unsigned> d_index;
    unsigned d_bxdf_count = 0;
};

} // End namespace llosl

#endif
