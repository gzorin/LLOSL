#include "BXDFScopeImpl.h"
#include "LLOSLContextImpl.h"

#include <llosl/BXDFScope.h>
#include <llosl/ShaderGroup.h>

namespace llosl {

BXDFScopeImpl::BXDFScopeImpl() {
}

BXDFScopeImpl::~BXDFScopeImpl() {
}

void
BXDFScopeImpl::insertBXDFsFromShaderGroup(const ShaderGroup& shader_group) {
    std::cerr << __PRETTY_FUNCTION__ << std::endl;

    auto path_count = shader_group.path_count();

    std::vector<unsigned> bxdf_mapping(path_count);

    for (unsigned path_id = 0; path_id < path_count; ++path_id) {
        const auto& bxdf = shader_group.getBXDFForPath(path_id);
        auto encoding = BXDFAST::encode(bxdf->ast);

        auto [ it, inserted ] = d_index.insert({ encoding, 0 });

        if (inserted) {
            it->second = d_bxdf_count++;;
        }

        bxdf_mapping[path_id] = it->second;
    }

    std::cerr << d_bxdf_count << " unique BXDFs" << std::endl;

    for (auto bxdf_id : bxdf_mapping) {
        std::cerr << bxdf_id << " ";
    }
    std::cerr << std::endl;
}

BXDFScope::BXDFScope(LLOSLContext *context)
: d_context(context ? &LLOSLContextImpl::Get(*context) : nullptr)
, d_impl(std::make_unique<BXDFScopeImpl>()) {
    if (d_context) {
        d_context->addBXDFScope(this);
    }
}

BXDFScope::~BXDFScope() {
    if (d_context) {
	    d_context->removeBXDFScope(this);
    }
}

void
BXDFScope::removeFromContext() {
    if (d_context) {
	    d_context->removeBXDFScope(this);
        d_context = nullptr;
    }
}

void
BXDFScope::insertBXDFsFromShaderGroup(const ShaderGroup& shader_group) {
    d_impl->insertBXDFsFromShaderGroup(shader_group);
}

} // End namespace llosl
