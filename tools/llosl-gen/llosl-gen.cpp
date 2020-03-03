#include <llosl/Builder.h>
#include <llosl/LLOSLContext.h>
#include <llosl/Shader.h>

#include <llvm/Bitcode/BitcodeWriter.h>
#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/Module.h>
#include <llvm/Support/CommandLine.h>
#include <llvm/Support/Error.h>
#include <llvm/Support/FileSystem.h>
#include <llvm/Support/MemoryBuffer.h>
#include <llvm/Support/ToolOutputFile.h>

namespace {

llvm::cl::opt<std::string> input_filename(llvm::cl::Positional, llvm::cl::desc("<input .oso file>"),
                                          llvm::cl::init("-"));

llvm::cl::opt<std::string> output_filename("o", llvm::cl::desc("Override output filename"),
                                           llvm::cl::value_desc("filename"));

} // namespace

//
int
main(int argc, char **argv) {
    llvm::cl::ParseCommandLineOptions(argc, argv, ".oso -> .bc translator\n");

    llvm::ExitOnError exit_on_err("llosl-gen: ");

    if (output_filename.empty()) {
        if (input_filename == "-") {
            output_filename = "-";
        }
        else {
            llvm::StringRef IFN = input_filename;
            output_filename     = (IFN.endswith(".bc") ? IFN.drop_back(3) : IFN).str();
            output_filename += ".bc";
        }
    }

    std::unique_ptr<llvm::LLVMContext>   llvm_context = std::make_unique<llvm::LLVMContext>();
    std::unique_ptr<llosl::LLOSLContext> llosl_context =
        std::make_unique<llosl::LLOSLContext>(*llvm_context, 1);

    auto shader_result = llosl_context->createShaderFromFile(input_filename);

    if (!shader_result) {
        llvm::errs() << llvm::toString(shader_result.takeError());
        return 1;
    }

    auto shader = std::move(*shader_result);

    // Write it out:
    std::error_code                       error_code;
    std::unique_ptr<llvm::ToolOutputFile> output_file(
        new llvm::ToolOutputFile(output_filename, error_code, llvm::sys::fs::F_None));

    if (error_code) {
        llvm::errs() << error_code.message();
        return 1;
    }

    llvm::WriteBitcodeToFile(shader->module(), output_file->os());
    output_file->keep();

    return 0;
}
