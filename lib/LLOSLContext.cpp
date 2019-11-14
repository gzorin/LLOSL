#include <llosl/Builder.h>
#include <llosl/BXDF.h>
#include <llosl/LLOSLContext.h>
#include <llosl/Shader.h>
#include <llosl/ShaderGroup.h>
#include <llosl/UberBXDF.h>

#include <llvm/ADT/APFloat.h>
#include <llvm/ADT/APInt.h>
#include <llvm/IR/Constants.h>
#include <llvm/IR/DerivedTypes.h>
#include <llvm/IR/Function.h>
#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/Module.h>

#include <OSL/genclosure.h>

#include "LLOSLContextImpl.h"

#include <map>
#include <utility>

std::size_t
std::hash<OSL::pvt::ShaderMaster::ref>::operator()(OSL::pvt::ShaderMaster::ref p) const {
    return std::hash<OSL::pvt::ShaderMaster*>()(p.get());
}

namespace llosl {

// Implementation:
LLOSLContextImpl::LLOSLContextImpl(llvm::LLVMContext& llcontext, unsigned bxdf_address_space)
    : d_llcontext(llcontext)
    , d_shading_system(new OSL::ShadingSystem(this, nullptr, &d_osl_error_handler))
    , d_shading_context(d_shading_system->get_context(d_shading_system->create_thread_info()))
    , d_bxdf_address_space(bxdf_address_space) {
    d_shading_system->attribute("lockgeom", 0);
    d_shading_system->attribute("optimize", 0);

    registerClosures();
}

LLOSLContextImpl::~LLOSLContextImpl() {
    assert(!d_builder);

    d_uber_bxdfs.clear();
    d_bxdfs.clear();
    d_shaders.clear();
    d_shader_groups.clear();
}

llvm::Type *
LLOSLContextImpl::getLLVMType(const OSL::TypeDesc& t) {
    OSL::TypeDesc::BASETYPE  basetype  = (OSL::TypeDesc::BASETYPE)t.basetype;
    OSL::TypeDesc::AGGREGATE aggregate = (OSL::TypeDesc::AGGREGATE)t.aggregate;
    auto arraylen  = t.arraylen;

    llvm::Type *llvm_type = nullptr;

    if (arraylen > 0) {
        return llvm::ArrayType::get(
            getLLVMType(OSL::TypeDesc(basetype, aggregate)), arraylen);
    }

    if (aggregate != OSL::TypeDesc::SCALAR) {
        switch (aggregate) {
            case OSL::TypeDesc::VEC2:
                return llvm::VectorType::get(
                    getLLVMType(OSL::TypeDesc(basetype)), 2);
            case OSL::TypeDesc::VEC3:
                return llvm::VectorType::get(
                    getLLVMType(OSL::TypeDesc(basetype)), 3);
            case OSL::TypeDesc::VEC4:
                return llvm::VectorType::get(
                    getLLVMType(OSL::TypeDesc(basetype)), 4);
            case OSL::TypeDesc::MATRIX33:
                return llvm::StructType::get(d_llcontext,
                    std::vector<llvm::Type *>{
                        getLLVMType(OSL::TypeDesc(basetype, OSL::TypeDesc::VEC3, 3))
                    });
            case OSL::TypeDesc::MATRIX44:
                return llvm::StructType::get(d_llcontext,
                    std::vector<llvm::Type *>{
                        getLLVMType(OSL::TypeDesc(basetype, OSL::TypeDesc::VEC4, 4))
                    });
            default:
                return nullptr;
        }
    }

    switch (basetype) {
        case OSL::TypeDesc::CHAR:
        case OSL::TypeDesc::UCHAR:
            return llvm::Type::getInt8Ty(d_llcontext);
        case OSL::TypeDesc::SHORT:
        case OSL::TypeDesc::USHORT:
            return llvm::Type::getInt16Ty(d_llcontext);
        case OSL::TypeDesc::INT:
        case OSL::TypeDesc::UINT:
            return llvm::Type::getInt32Ty(d_llcontext);
        case OSL::TypeDesc::LONGLONG:
        case OSL::TypeDesc::ULONGLONG:
            return llvm::Type::getInt64Ty(d_llcontext);
        case OSL::TypeDesc::HALF:
            return llvm::Type::getHalfTy(d_llcontext);
        case OSL::TypeDesc::FLOAT:
            return llvm::Type::getFloatTy(d_llcontext);
        case OSL::TypeDesc::DOUBLE:
            return llvm::Type::getDoubleTy(d_llcontext);
        case OSL::TypeDesc::STRING:
            if (!d_string_type) {
                d_string_type = llvm::StructType::create(
                    d_llcontext,
                    std::vector<llvm::Type *>{
                        llvm::Type::getInt32Ty(d_llcontext)
                    },
                    "OSL::String");
            }

            return d_string_type;
        case OSL::TypeDesc::PTR:
            return llvm::PointerType::get(
                llvm::Type::getInt8Ty(d_llcontext), 0);
        default:
            return nullptr;
    }

    return nullptr;
}

llvm::Constant *
LLOSLContextImpl::getLLVMDefaultConstant(const OSL::TypeDesc& t) {
    OSL::TypeDesc::BASETYPE  basetype  = (OSL::TypeDesc::BASETYPE)t.basetype;
    OSL::TypeDesc::AGGREGATE aggregate = (OSL::TypeDesc::AGGREGATE)t.aggregate;
    auto arraylen  = t.arraylen;

    llvm::Type *llvm_type = getLLVMType(t);

    if (arraylen > 0) {
        return llvm::ConstantArray::get(
            llvm::cast<llvm::ArrayType>(llvm_type),
            std::vector<llvm::Constant *>(
                arraylen, getLLVMDefaultConstant(OSL::TypeDesc(basetype, aggregate))));
    }

    if (aggregate != OSL::TypeDesc::SCALAR) {
        switch (aggregate) {
            case OSL::TypeDesc::VEC2:
                return llvm::ConstantVector::get(
                    std::vector<llvm::Constant *>(2, getLLVMDefaultConstant(OSL::TypeDesc(basetype))));
            case OSL::TypeDesc::VEC3:
                return llvm::ConstantVector::get(
                    std::vector<llvm::Constant *>(3, getLLVMDefaultConstant(OSL::TypeDesc(basetype))));
            case OSL::TypeDesc::VEC4:
                return llvm::ConstantVector::get(
                    std::vector<llvm::Constant *>(4, getLLVMDefaultConstant(OSL::TypeDesc(basetype))));
            case OSL::TypeDesc::MATRIX33:
                return llvm::ConstantStruct::get(
                    llvm::cast<llvm::StructType>(llvm_type),
                    std::vector<llvm::Constant *>{
                        getLLVMDefaultConstant(OSL::TypeDesc(basetype, OSL::TypeDesc::VEC3, 3))
                    });
            case OSL::TypeDesc::MATRIX44:
                return llvm::ConstantStruct::get(
                    llvm::cast<llvm::StructType>(llvm_type),
                    std::vector<llvm::Constant *>{
                        getLLVMDefaultConstant(OSL::TypeDesc(basetype, OSL::TypeDesc::VEC4, 4))
                    });
            default:
                return nullptr;
        }
    }

    switch (basetype) {
        case OSL::TypeDesc::CHAR:
        case OSL::TypeDesc::SHORT:
        case OSL::TypeDesc::INT:
        case OSL::TypeDesc::LONGLONG:
            return llvm::ConstantInt::get(llvm_type, 0, true);
        case OSL::TypeDesc::UCHAR:
        case OSL::TypeDesc::USHORT:
        case OSL::TypeDesc::UINT:
        case OSL::TypeDesc::ULONGLONG:
            return llvm::ConstantInt::get(llvm_type, 0, false);
        case OSL::TypeDesc::HALF:
        case OSL::TypeDesc::FLOAT:
        case OSL::TypeDesc::DOUBLE:
            return llvm::ConstantFP::get(llvm_type, 0.0f);
        case OSL::TypeDesc::STRING:
            return llvm::ConstantStruct::get(
                llvm::cast<llvm::StructType>(llvm_type),
                std::vector<llvm::Constant *>{
                    llvm::ConstantInt::get(
                        llvm::Type::getInt32Ty(d_llcontext), 0xFFFF)
                });
        case OSL::TypeDesc::PTR:
            return llvm::ConstantPointerNull::get(
                llvm::cast<llvm::PointerType>(llvm_type));
        default:
            return nullptr;
    }

    return nullptr;
}

std::pair<llvm::Constant *, const void *>
LLOSLContextImpl::getLLVMConstant(const OSL::TypeDesc& t, const void *p) {
    OSL::TypeDesc::BASETYPE  basetype  = (OSL::TypeDesc::BASETYPE)t.basetype;
    OSL::TypeDesc::AGGREGATE aggregate = (OSL::TypeDesc::AGGREGATE)t.aggregate;
    auto arraylen  = t.arraylen;

    llvm::Type *llvm_type = getLLVMType(t);

    if (arraylen > 0) {
        std::vector<llvm::Constant *> elements(arraylen, nullptr);

        std::generate_n(
            elements.begin(), arraylen,
            [this, &p, basetype, aggregate]() -> llvm::Constant * {
                auto [ element, next_p ] = getLLVMConstant(OSL::TypeDesc(basetype, aggregate), p);
                p = next_p;
                return element;
            });

        return {
            llvm::ConstantArray::get(
                llvm::cast<llvm::ArrayType>(llvm_type), elements),
            p
        };
    }

    if (aggregate != OSL::TypeDesc::SCALAR) {
        switch (aggregate) {
            case OSL::TypeDesc::VEC2:
            case OSL::TypeDesc::VEC3:
            case OSL::TypeDesc::VEC4: {
                unsigned n = 0;
                switch (aggregate) {
                    case OSL::TypeDesc::VEC2: n = 2; break;
                    case OSL::TypeDesc::VEC3: n = 3; break;
                    case OSL::TypeDesc::VEC4: n = 4; break;
                    default: break;
                }

                std::vector<llvm::Constant *> elements(n, nullptr);

                std::generate_n(
                    elements.begin(), n,
                    [this, &p, basetype, aggregate]() -> llvm::Constant * {
                        auto [ element, next_p ] = getLLVMConstant(OSL::TypeDesc(basetype), p);
                        p = next_p;
                        return element;
                    });

                return {
                    llvm::ConstantVector::get(elements),
                    p
                };
            }
            case OSL::TypeDesc::MATRIX33:
            case OSL::TypeDesc::MATRIX44: {
                unsigned n = 0;
                OSL::TypeDesc::AGGREGATE column_aggregate = OSL::TypeDesc::SCALAR;
                switch (aggregate) {
                    case OSL::TypeDesc::MATRIX33: n = 3; column_aggregate = OSL::TypeDesc::VEC3; break;
                    case OSL::TypeDesc::MATRIX44: n = 4; column_aggregate = OSL::TypeDesc::VEC4; break;
                    default: break;
                }

                auto [ value, next_p ] = getLLVMConstant(OSL::TypeDesc(basetype, column_aggregate, n), p);

                return {
                    llvm::ConstantStruct::get(
                        llvm::cast<llvm::StructType>(llvm_type),
                        std::vector<llvm::Constant *>{
                            value
                        }),
                    next_p
                };
            }
            default:
                return { nullptr, p };
        }
    }

    switch (basetype) {
        case OSL::TypeDesc::CHAR: {
            auto pdata = reinterpret_cast<const int8_t *>(p);
            return {
                llvm::ConstantInt::get(llvm_type, *pdata, true),
                ++pdata
            };
        }
        case OSL::TypeDesc::SHORT: {
            auto pdata = reinterpret_cast<const int16_t *>(p);
            return {
                llvm::ConstantInt::get(llvm_type, *pdata, true),
                ++pdata
            };
        }
        case OSL::TypeDesc::INT: {
            auto pdata = reinterpret_cast<const int32_t *>(p);
            return {
                llvm::ConstantInt::get(llvm_type, *pdata, true),
                ++pdata
            };
        }
        case OSL::TypeDesc::LONGLONG: {
            auto pdata = reinterpret_cast<const int64_t *>(p);
            return {
                llvm::ConstantInt::get(llvm_type, *pdata, true),
                ++pdata
            };
        }
        case OSL::TypeDesc::UCHAR: {
            auto pdata = reinterpret_cast<const uint8_t *>(p);
            return {
                llvm::ConstantInt::get(llvm_type, *pdata, false),
                ++pdata
            };
        }
        case OSL::TypeDesc::USHORT: {
            auto pdata = reinterpret_cast<const uint16_t *>(p);
            return {
                llvm::ConstantInt::get(llvm_type, *pdata, false),
                ++pdata
            };
        }
        case OSL::TypeDesc::UINT: {
            auto pdata = reinterpret_cast<const uint32_t *>(p);
            return {
                llvm::ConstantInt::get(llvm_type, *pdata, false),
                ++pdata
            };
        }
        case OSL::TypeDesc::ULONGLONG: {
            auto pdata = reinterpret_cast<const uint64_t *>(p);
            return {
                llvm::ConstantInt::get(llvm_type, *pdata, false),
                ++pdata
            };
        }
        case OSL::TypeDesc::HALF: {
            auto pdata = reinterpret_cast<const uint16_t *>(p);
            return {
                llvm::ConstantFP::get(d_llcontext,
                    llvm::APFloat(llvm::APFloat::IEEEhalf(), llvm::APInt(16, *pdata, false))),
                ++pdata
            };
        }
        case OSL::TypeDesc::FLOAT: {
            auto pdata = reinterpret_cast<const uint32_t *>(p);
            return {
                llvm::ConstantFP::get(d_llcontext,
                    llvm::APFloat(llvm::APFloat::IEEEsingle(), llvm::APInt(32, *pdata, false))),
                ++pdata
            };
        }
        case OSL::TypeDesc::DOUBLE: {
            auto pdata = reinterpret_cast<const uint64_t *>(p);
            return {
                llvm::ConstantFP::get(d_llcontext,
                    llvm::APFloat(llvm::APFloat::IEEEdouble(), llvm::APInt(64, *pdata, false))),
                ++pdata
            };
        }
        case OSL::TypeDesc::STRING:
            // TODO
            return {
                llvm::ConstantStruct::get(
                    llvm::cast<llvm::StructType>(llvm_type),
                    std::vector<llvm::Constant *>{
                        llvm::ConstantInt::get(
                            llvm::Type::getInt32Ty(d_llcontext), 0xFFFF)
                    }),
                p
            };
        case OSL::TypeDesc::PTR:
            // TODO
            return {
                llvm::ConstantPointerNull::get(
                    llvm::cast<llvm::PointerType>(llvm_type)),
                p
            };
        default:
            return { nullptr, p };
    }

    return { nullptr, p };
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

#if 0
        llvm::Type *params_type = nullptr;

        d_shading_system->query_closure(
            nullptr, &builtins[i].id, nullptr, &params_type);

        auto bxdf_component_type = llvm::FunctionType::get(
            float3_type,
            std::vector<llvm::Type *>{
                float3_type, float3_type,
                llvm::PointerType::get(params_type, d_bxdf_address_space) },
            false);

        std::string name = "llosl_" + std::string(builtins[i].name);

        d_bxdf_components.insert({
            builtins[i].id,
            llvm::Function::Create(
                bxdf_component_type, llvm::GlobalValue::ExternalLinkage,
                name, d_bxdf_module.get()) });
#endif
    }

    d_uber_bxdf = new UberBXDF(*this);
}

// OSL::RendererServices overrides:
#if 0
llvm::LLVMContext *
LLOSLContextImpl::llvm_context() const {
    return &d_llcontext;
}
#endif

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

void
LLOSLContextImpl::addShader(Shader *shader) {
    d_shaders.push_back(shader);
}

void
LLOSLContextImpl::removeShader(Shader *shader) {
    d_shaders.remove(shader);
}

llvm::Expected<Shader *>
LLOSLContextImpl::getShaderFromShaderMaster(OSL::pvt::ShaderMaster::ref shader_master) {
    auto it = d_shader_masters.find(shader_master);
    if (it != d_shader_masters.end()) {
        return it->second;
    }

    auto shader = new Shader(*this, *shader_master);
    d_shader_masters.insert({ shader_master, shader });

    return { shader };
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

LLOSLContext::LLOSLContext(llvm::LLVMContext& llcontext, unsigned bxdf_address_space)
  : d_impl(new LLOSLContextImpl(llcontext, bxdf_address_space)) {
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

llvm::Type *
LLOSLContext::getLLVMType(const OSL::TypeDesc& t) {
    return d_impl->getLLVMType(t);
}

llvm::Constant *
LLOSLContext::getLLVMDefaultConstant(const OSL::TypeDesc& t) {
    return d_impl->getLLVMDefaultConstant(t);
}

std::pair<llvm::Constant *, const void *>
LLOSLContext::getLLVMConstant(const OSL::TypeDesc& t, const void *p) {
    return d_impl->getLLVMConstant(t, p);
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
