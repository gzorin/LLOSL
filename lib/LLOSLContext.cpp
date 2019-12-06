#include <llosl/Builder.h>
#include <llosl/BXDF.h>
#include <llosl/Closure.h>
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
#include <llvm/Support/FormatVariadic.h>

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

template<typename ResultType, typename ScalarFunction, typename VectorFunction, typename MatrixFunction, typename ArrayFunction>
std::optional<ResultType>
processType(const OSL::TypeDesc& t, bool packed, ScalarFunction scalar, VectorFunction vector, MatrixFunction matrix, ArrayFunction array) {
    OSL::TypeDesc::BASETYPE  basetype  = (OSL::TypeDesc::BASETYPE)t.basetype;
    OSL::TypeDesc::AGGREGATE aggregate = (OSL::TypeDesc::AGGREGATE)t.aggregate;
    auto arraylen  = t.arraylen;

    if (arraylen > 0) {
        return array(OSL::TypeDesc(basetype, aggregate), packed, arraylen);
    }

    if (aggregate != OSL::TypeDesc::SCALAR) {
        unsigned n = 1;
        OSL::TypeDesc::AGGREGATE column_aggregate = OSL::TypeDesc::SCALAR;

        switch (aggregate) {
            case OSL::TypeDesc::VEC2    : n = 2; break;
            case OSL::TypeDesc::VEC3    : n = 3; break;
            case OSL::TypeDesc::VEC4    : n = 4; break;
            case OSL::TypeDesc::MATRIX33: n = 3; column_aggregate = OSL::TypeDesc::VEC3; break;
            case OSL::TypeDesc::MATRIX44: n = 4; column_aggregate = OSL::TypeDesc::VEC4; break;
            default: break;
        }

        switch (aggregate) {
            case OSL::TypeDesc::VEC2:
            case OSL::TypeDesc::VEC3:
            case OSL::TypeDesc::VEC4:
                return vector(OSL::TypeDesc(basetype), n, packed);
            case OSL::TypeDesc::MATRIX33:
            case OSL::TypeDesc::MATRIX44:
                return matrix(OSL::TypeDesc(basetype, column_aggregate), packed, n);
            default:
                return std::optional<ResultType>();
        }
    }

    return scalar(OSL::TypeDesc(basetype));
}

llvm::Type *
LLOSLContextImpl::getLLVMType(const OSL::TypeDesc& t, bool packed) {
    auto result = processType<llvm::Type *>(t, packed,
        [this](auto t) -> llvm::Type * {
            switch (t.basetype) {
                case OSL::TypeDesc::NONE:
                    return llvm::Type::getVoidTy(d_llcontext);
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
                    return getLLVMStringType();
                case OSL::TypeDesc::PTR:
                    return llvm::PointerType::get(
                        llvm::Type::getInt8Ty(d_llcontext), 0);
                default:
                    return nullptr;
            }
        },
        [this](auto t, auto n, auto packed) -> llvm::Type * {
            auto element_type = getLLVMType(t, false);

            return packed
                ? (llvm::Type *)llvm::ArrayType::get(element_type, n)
                : (llvm::Type *)llvm::VectorType::get(element_type, n);
        },
        [this](auto t, auto packed, auto n) -> llvm::Type * {
            return llvm::StructType::get(d_llcontext,
                std::vector<llvm::Type *>{
                    llvm::ArrayType::get(getLLVMType(t, packed), n)
                });
        },
        [this](auto t, auto packed, auto n) -> llvm::Type * {
            return llvm::ArrayType::get(getLLVMType(t, packed), n);
        });

    assert(result);
    return *result;
}

llvm::Constant *
LLOSLContextImpl::getLLVMDefaultConstant(const OSL::TypeDesc& t, bool packed) {
    llvm::Type *llvm_type = getLLVMType(t, packed);

    auto getLLVMDefaultConstants = [this](unsigned n, const OSL::TypeDesc& t, bool packed = false) -> std::vector<llvm::Constant *> {
        return std::vector<llvm::Constant *>(n, getLLVMDefaultConstant(t, packed));
    };

    auto result = processType<llvm::Constant *>(t, packed,
        [this, llvm_type](auto t) -> llvm::Constant * {
            switch (t.basetype) {
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
        },
        [this, llvm_type, getLLVMDefaultConstants](auto t, auto n, auto packed) -> llvm::Constant * {
            auto scalar_constants = getLLVMDefaultConstants(n, t);

            return packed
                ? llvm::ConstantArray::get(llvm::cast<llvm::ArrayType>(llvm_type), scalar_constants)
                : llvm::ConstantVector::get(scalar_constants);
        },
        [this, llvm_type, getLLVMDefaultConstants](auto t, auto packed, auto n) -> llvm::Constant * {
            auto column_constants = getLLVMDefaultConstants(n, t, packed);

            return llvm::ConstantStruct::get(
                llvm::cast<llvm::StructType>(llvm_type),
                std::vector<llvm::Constant *>{
                    llvm::ConstantArray::get(
                        llvm::ArrayType::get(
                            getLLVMType(t, packed),
                            n),
                        column_constants)
                });
        },
        [this, llvm_type, getLLVMDefaultConstants](auto t, auto packed, auto n) -> llvm::Constant * {
            auto element_constants = getLLVMDefaultConstants(n, t, packed);

            return llvm::ConstantArray::get(
                llvm::cast<llvm::ArrayType>(llvm_type),
                element_constants);
        });

    assert(result);
    return *result;
}

std::pair<llvm::Constant *, const void *>
LLOSLContextImpl::getLLVMConstant(const OSL::TypeDesc& t, const void *p, bool packed) {
    using ResultType = std::pair<llvm::Constant *, const void *>;

    llvm::Type *llvm_type = getLLVMType(t, packed);

    auto getLLVMConstants = [this](const void *p, unsigned n, const OSL::TypeDesc& t, bool packed = false) -> std::pair<std::vector<llvm::Constant *>, const void *> {
        std::vector<llvm::Constant *> constants;
        constants.reserve(n);

        for (; n > 0; --n) {
            auto [ constant, next_p ] = getLLVMConstant(t, p, packed);
            constants.push_back(constant);
            p = next_p;
        }

        return { constants, p };
    };

    auto result = processType<ResultType>(t, packed,
        [this, p, llvm_type](auto t) -> ResultType {
            switch (t.basetype) {
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
        },
        [this, p, llvm_type, getLLVMConstants](auto t, auto n, auto packed) -> ResultType {
            auto [ scalars, next_p ] = getLLVMConstants(p, n, t);

            return {
                packed
                    ? (llvm::Constant *)llvm::ConstantArray::get(llvm::cast<llvm::ArrayType>(llvm_type), scalars)
                    : (llvm::Constant *)llvm::ConstantVector::get(scalars),
                next_p
            };
        },
        [this, p, llvm_type, getLLVMConstants](auto t, auto packed, auto n) -> ResultType {
            auto [ columns, next_p ] = getLLVMConstants(p, n, t, packed);

            return {
                llvm::ConstantStruct::get(
                    llvm::cast<llvm::StructType>(llvm_type),
                    std::vector<llvm::Constant *>{
                        llvm::ConstantArray::get(
                            llvm::ArrayType::get(
                                getLLVMType(t, packed),
                                n),
                            columns)
                    }),
                next_p
            };
        },
        [this, p, llvm_type, getLLVMConstants](auto t, auto packed, auto n) -> ResultType {
            auto [ elements, next_p ] = getLLVMConstants(p, n, t, packed);

            return {
                llvm::ConstantArray::get(
                    llvm::cast<llvm::ArrayType>(llvm_type), elements),
                next_p
            };
        });

    assert(result);
    return *result;
}

bool
LLOSLContextImpl::isTypePassedByReference(const OSL::TypeDesc& t) const {
    OSL::TypeDesc::BASETYPE  basetype  = (OSL::TypeDesc::BASETYPE)t.basetype;
    OSL::TypeDesc::AGGREGATE aggregate = (OSL::TypeDesc::AGGREGATE)t.aggregate;
    auto arraylen  = t.arraylen;

    return arraylen > 0 ||
           aggregate == OSL::TypeDesc::MATRIX33 || aggregate == OSL::TypeDesc::MATRIX44 ||
           basetype == OSL::TypeDesc::STRING;
}

llvm::Type *
LLOSLContextImpl::getLLVMTypeForArgument(const OSL::TypeDesc& t, bool packed) {
    OSL::TypeDesc::BASETYPE  basetype  = (OSL::TypeDesc::BASETYPE)t.basetype;
    OSL::TypeDesc::AGGREGATE aggregate = (OSL::TypeDesc::AGGREGATE)t.aggregate;
    auto arraylen  = t.arraylen;

    if (arraylen > 0 ||
        aggregate == OSL::TypeDesc::MATRIX33 || aggregate == OSL::TypeDesc::MATRIX44 ||
        basetype == OSL::TypeDesc::STRING) {
        return llvm::PointerType::get(getLLVMType(t, packed), 0);
    }

    return getLLVMType(t, packed);
}

llvm::StructType *
LLOSLContextImpl::getLLVMStringType() {
    if (!d_string_type) {
        d_string_type = llvm::StructType::create(
            d_llcontext,
            std::vector<llvm::Type *>{
                llvm::Type::getInt32Ty(d_llcontext)
            },
            "OSL::String");
    }

    return d_string_type;
}

llvm::StructType *
LLOSLContextImpl::getLLVMClosureType() {
    if (!d_closure_type) {
        d_closure_type = llvm::StructType::create(
            d_llcontext,
            std::vector<llvm::Type *>{
                getLLVMClosurePointerType()
            },
            "OSL::Closure");
    }

    return d_closure_type;
}

llvm::Constant *
LLOSLContextImpl::getLLVMClosureDefaultConstant() {
    return llvm::ConstantStruct::get(
        getLLVMClosureType(),
        std::vector<llvm::Constant*>{ getLLVMClosurePointerDefaultConstant() });
}

llvm::PointerType *
LLOSLContextImpl::getLLVMClosurePointerType() {
    return llvm::PointerType::get(
        llvm::Type::getInt8Ty(d_llcontext), d_bxdf_address_space);
}

llvm::Constant *
LLOSLContextImpl::getLLVMClosurePointerDefaultConstant() {
    return llvm::ConstantPointerNull::get(getLLVMClosurePointerType());
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
        //{ "reflection" , REFLECTION_ID,         { CLOSURE_VECTOR_PARAM(ReflectionParams, N),
        //                                          CLOSURE_FINISH_PARAM(ReflectionParams) } },
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

    for (int i = 0; builtins[i].name; i++) {
        auto  closure_name   = builtins[i].name;
        auto  closure_id     = builtins[i].id;
        auto& closure_params = builtins[i].params;

        d_shading_system->register_closure(
            closure_name,
            closure_id,
            closure_params,
            nullptr, nullptr);

        auto param_begin = &closure_params[0];
        auto param_end   = std::find_if(
            param_begin, &closure_params[MaxParams],
            [](const auto& param) -> bool {
                return !param.type;
            });

        auto closure = Closure::create(*this, closure_name, closure_id, { param_begin, param_end }, d_bxdf_module.get());
        d_closures_by_name.insert({ closure->name(), closure->id() });
        d_closures.insert({ closure->id(), std::move(closure) });
    }

    d_uber_bxdf = new UberBXDF(*this);
}

// OSL::RendererServices overrides:
int
LLOSLContextImpl::supports(OSL::string_view feature) const {
    if (feature == "LLOSL") {
	return true;
    }

    return false;
}

//
const Closure *
LLOSLContextImpl::getClosure(unsigned id) const {
    auto it = d_closures.find(id);

    if (it == d_closures.end()) {
        return nullptr;
    }

    return it->second.get();
}

const Closure *
LLOSLContextImpl::getClosure(llvm::StringRef name) const {
    auto it = d_closures_by_name.find(name);

    if (it == d_closures_by_name.end()) {
        return nullptr;
    }

    return getClosure(it->second);
}

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
LLOSLContextImpl::createShaderFromFile(llvm::StringRef filename) {
    auto osl_error_scope = enterOSLErrorScope();

    auto shader_master = d_shading_context->shadingsys().loadshader(OSL::string_view(filename.data(), filename.size()));

    auto error = osl_error_scope.takeError();

    if (error) {
        return llvm::Expected<Shader *>(std::move(error));
    }

    assert(shader_master);

    return llvm::Expected<Shader *>(getShaderFromShaderMaster(shader_master));
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
LLOSLContext::getLLVMType(const OSL::TypeDesc& t, bool packed) {
    return d_impl->getLLVMType(t, packed);
}

llvm::Constant *
LLOSLContext::getLLVMDefaultConstant(const OSL::TypeDesc& t, bool packed) {
    return d_impl->getLLVMDefaultConstant(t, packed);
}

std::pair<llvm::Constant *, const void *>
LLOSLContext::getLLVMConstant(const OSL::TypeDesc& t, const void *p, bool packed) {
    return d_impl->getLLVMConstant(t, p, packed);
}

llvm::Expected<Shader *>
LLOSLContext::createShaderFromFile(llvm::StringRef filename) {
    return d_impl->createShaderFromFile(filename);
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
