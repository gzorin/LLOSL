#include <llosl/Builder.h>
#include <llosl/BXDF.h>
#include <llosl/LLOSLContext.h>
#include <llosl/ShaderGroup.h>
#include <llosl/UberBXDF.h>

#include <llvm/IR/Function.h>
#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/Module.h>
#include <llvm/IR/DerivedTypes.h>

#include <OSL/genclosure.h>

#include "LLOSLContextImpl.h"

#include <map>
#include <utility>

namespace llosl {

// Implementation:
LLOSLContextImpl::LLOSLContextImpl(llvm::LLVMContext& llcontext)
    : d_llcontext(llcontext)
    , d_shading_system(new OSL::ShadingSystem(this, nullptr, &d_osl_error_handler))
    , d_shading_context(d_shading_system->get_context(d_shading_system->create_thread_info())) {
    d_shading_system->attribute("lockgeom", 0);
    d_shading_system->attribute("optimize", 0);

    registerClosures();
}

LLOSLContextImpl::~LLOSLContextImpl() {
    assert(!d_builder);

    d_uber_bxdfs.clear();
    d_bxdfs.clear();
    d_shader_groups.clear();
}

void
LLOSLContextImpl::registerClosures() {
    d_bxdf_module.reset(new llvm::Module("llosl.bxdf", d_llcontext));

    enum ClosureIDs {
        EMISSION_ID = 1,
        BACKGROUND_ID,
        DIFFUSE_ID,
        OREN_NAYAR_ID,
        TRANSLUCENT_ID,
        PHONG_ID,
        WARD_ID,
        MICROFACET_ID,
        REFLECTION_ID,
        FRESNEL_REFLECTION_ID,
        REFRACTION_ID,
        TRANSPARENT_ID,
    };

    struct EmptyParams      { };
    struct DiffuseParams    { OSL::Vec3 N; };
    struct OrenNayarParams  { OSL::Vec3 N; float sigma; };
    struct PhongParams      { OSL::Vec3 N; float exponent; };
    struct WardParams       { OSL::Vec3 N, T; float ax, ay; };
    struct ReflectionParams { OSL::Vec3 N; float eta; };
    struct RefractionParams { OSL::Vec3 N; float eta; };
    struct MicrofacetParams { OSL::ustring dist; OSL::Vec3 N, U; float xalpha, yalpha, eta; int refract; };

    using namespace OSL;

    // Describe the memory layout of each closure type to the OSL runtime
    enum { MaxParams = 32 };
    struct BuiltinClosures {
        const char* name;
        int id;
        ClosureParam params[MaxParams]; // upper bound
    };
    BuiltinClosures builtins[] = {
        { "emission"   , EMISSION_ID,           { CLOSURE_FINISH_PARAM(EmptyParams) } },
        { "background" , BACKGROUND_ID,         { CLOSURE_FINISH_PARAM(EmptyParams) } },
        { "diffuse"    , DIFFUSE_ID,            { CLOSURE_VECTOR_PARAM(DiffuseParams, N),
                                                  CLOSURE_FINISH_PARAM(DiffuseParams) } },
        { "oren_nayar" , OREN_NAYAR_ID,         { CLOSURE_VECTOR_PARAM(OrenNayarParams, N),
                                                  CLOSURE_FLOAT_PARAM (OrenNayarParams, sigma),
                                                  CLOSURE_FINISH_PARAM(OrenNayarParams) } },
        { "translucent", TRANSLUCENT_ID,        { CLOSURE_VECTOR_PARAM(DiffuseParams, N),
                                                  CLOSURE_FINISH_PARAM(DiffuseParams) } },
        { "phong"      , PHONG_ID,              { CLOSURE_VECTOR_PARAM(PhongParams, N),
                                                  CLOSURE_FLOAT_PARAM (PhongParams, exponent),
                                                  CLOSURE_FINISH_PARAM(PhongParams) } },
        { "ward"       , WARD_ID,               { CLOSURE_VECTOR_PARAM(WardParams, N),
                                                  CLOSURE_VECTOR_PARAM(WardParams, T),
                                                  CLOSURE_FLOAT_PARAM (WardParams, ax),
                                                  CLOSURE_FLOAT_PARAM (WardParams, ay),
                                                  CLOSURE_FINISH_PARAM(WardParams) } },
        { "microfacet", MICROFACET_ID,          { CLOSURE_STRING_PARAM(MicrofacetParams, dist),
                                                  CLOSURE_VECTOR_PARAM(MicrofacetParams, N),
                                                  CLOSURE_VECTOR_PARAM(MicrofacetParams, U),
                                                  CLOSURE_FLOAT_PARAM (MicrofacetParams, xalpha),
                                                  CLOSURE_FLOAT_PARAM (MicrofacetParams, yalpha),
                                                  CLOSURE_FLOAT_PARAM (MicrofacetParams, eta),
                                                  CLOSURE_INT_PARAM   (MicrofacetParams, refract),
                                                  CLOSURE_FINISH_PARAM(MicrofacetParams) } },
        { "reflection" , REFLECTION_ID,         { CLOSURE_VECTOR_PARAM(ReflectionParams, N),
                                                  CLOSURE_FINISH_PARAM(ReflectionParams) } },
        { "reflection" , FRESNEL_REFLECTION_ID, { CLOSURE_VECTOR_PARAM(ReflectionParams, N),
                                                  CLOSURE_FLOAT_PARAM (ReflectionParams, eta),
                                                  CLOSURE_FINISH_PARAM(ReflectionParams) } },
        { "refraction" , REFRACTION_ID,         { CLOSURE_VECTOR_PARAM(RefractionParams, N),
                                                  CLOSURE_FLOAT_PARAM (RefractionParams, eta),
                                                  CLOSURE_FINISH_PARAM(RefractionParams) } },
        { "transparent", TRANSPARENT_ID,        { CLOSURE_FINISH_PARAM(EmptyParams) } },
        // mark end of the array
        { nullptr, 0, {} }
    };

    auto float3_type = llvm::VectorType::get(
        llvm::Type::getFloatTy(d_llcontext), 3);

    auto void_pointer_type = llvm::Type::getInt8PtrTy(
        d_llcontext);

    for (int i = 0; builtins[i].name; i++) {
        d_shading_system->register_closure(
            builtins[i].name,
            builtins[i].id,
            builtins[i].params,
            nullptr, nullptr);

        llvm::Type *params_type = nullptr;

        d_shading_system->query_closure(
            nullptr, &builtins[i].id, nullptr, &params_type);

        auto bxdf_component_type = llvm::FunctionType::get(
            float3_type,
            std::vector<llvm::Type *>{
                float3_type, float3_type,
                llvm::PointerType::get(params_type, 0) },
            false);

        std::string name = "llosl_" + std::string(builtins[i].name);

        d_bxdf_components.insert({
            builtins[i].id,
            llvm::Function::Create(
                bxdf_component_type, llvm::GlobalValue::ExternalLinkage,
                name, d_bxdf_module.get()) });
    }

    d_uber_bxdf = new UberBXDF(*this);
}

// OSL::RendererServices overrides:
llvm::LLVMContext *
LLOSLContextImpl::llvm_context() const {
    return &d_llcontext;
}

int
LLOSLContextImpl::supports(OSL::string_view feature) const {
    if (feature == "LLOSL") {
	return true;
    }

    return false;
}

//
OSLErrorScope
LLOSLContextImpl::enterOSLErrorScope() {
    return d_osl_error_handler.enter();
}

llvm::Expected<Builder>
LLOSLContextImpl::getBuilder() {
    if (d_builder) {
        return llvm::Expected<Builder>(
            llvm::errorCodeToError(
            make_error_code(LLOSLContext::Error::AlreadyBuilding)));
    }

    return llvm::Expected<Builder>(Builder(*this));
}

void
LLOSLContextImpl::resetBuilder(BuilderImpl *builder) {
    d_builder = builder;
}

std::tuple<const BXDF *, unsigned, bool>
LLOSLContextImpl::getOrInsertBXDF(BXDF::EncodingView encoding, BXDFAST::NodeRef ast, std::size_t heap_size) {
    // Add to the overall index:
    auto it = d_bxdf_index.find(encoding);
    bool inserted = false;
    if (it == d_bxdf_index.end()) {
        auto bxdf = new BXDF(*this, encoding, ast, heap_size);
        it = d_bxdf_index.insert({ BXDF::Encoding(encoding), bxdf }).first;
        inserted = true;
    }

    // Add to the current UberBXDF:
    auto tmp = d_uber_bxdf->insertBXDF(it->second);

    return { it->second, tmp.first, inserted || tmp.second };
}

llvm::Function *
LLOSLContextImpl::getBXDFComponent(unsigned id) const {
    auto it = d_bxdf_components.find(id);
    if (it == d_bxdf_components.end()) {
        return nullptr;
    }

    return it->second;
}

void
LLOSLContextImpl::addBXDF(BXDF *bxdf) {
    d_bxdfs.push_back(bxdf);
}

void
LLOSLContextImpl::removeBXDF(BXDF *bxdf) {
    d_bxdfs.remove(bxdf);
}

void
LLOSLContextImpl::addUberBXDF(UberBXDF *uber_bxdf) {
    d_uber_bxdfs.push_back(uber_bxdf);
}

void
LLOSLContextImpl::removeUberBXDF(UberBXDF *uber_bxdf) {
    d_uber_bxdfs.remove(uber_bxdf);
}

void
LLOSLContextImpl::addShaderGroup(ShaderGroup *shader_group) {
    d_shader_groups.push_back(shader_group);
}

void
LLOSLContextImpl::removeShaderGroup(ShaderGroup *shader_group) {
    d_shader_groups.remove(shader_group);
}

// Interface:
namespace {

namespace contexts {

std::map<llvm::LLVMContext*, LLOSLContext *>&
llvm_to_llosl() {
    static std::map<llvm::LLVMContext*, LLOSLContext *> s_llvm_to_llosl;
    return s_llvm_to_llosl;
}

} // End namespace contexts
} // End anonymous namespace

struct LLOSLContext::ErrorCategory : std::error_category {
    const char* name() const noexcept override {
	return "LLOSLContext";
    };

    std::string message(int ev) const override {
	switch (static_cast<Error>(ev)) {
	case Error::AlreadyBuilding:
	    return "already building";
	}
    }
};

const std::error_category&
LLOSLContext::ErrorCategory() {
    static struct ErrorCategory s_error_category;
    return s_error_category;
}

const LLOSLContext*
LLOSLContext::Get(const llvm::LLVMContext* llcontext) {
    auto it = contexts::llvm_to_llosl().find(const_cast<llvm::LLVMContext *>(llcontext));
    return it != contexts::llvm_to_llosl().end()?
	it->second :
	nullptr;
}

LLOSLContext*
LLOSLContext::Get(llvm::LLVMContext* llcontext) {
    auto it = contexts::llvm_to_llosl().find(llcontext);
    return it != contexts::llvm_to_llosl().end()?
	it->second :
	nullptr;
}

LLOSLContext::LLOSLContext(llvm::LLVMContext& llcontext)
  : d_impl(new LLOSLContextImpl(llcontext)) {
    contexts::llvm_to_llosl().insert(std::make_pair(&llcontext, this));
}

LLOSLContext::~LLOSLContext() {
    contexts::llvm_to_llosl().erase(&d_impl->getLLContext());
}

const llvm::LLVMContext&
LLOSLContext::getLLContext() const {
    return d_impl->getLLContext();
}

llvm::LLVMContext&
LLOSLContext::getLLContext() {
    return d_impl->getLLContext();
}

llvm::Expected<Builder>
LLOSLContext::getBuilder() {
    return d_impl->getBuilder();
}

UberBXDF *
LLOSLContext::getUberBXDF() const {
    return d_impl->uber_bxdf();
}

} // End namespace llosl
