//-*-C++-*-
#ifndef LLOSL_OSLERROR_H
#define LLOSL_OSLERROR_H

#include <llvm/Support/Error.h>

namespace llosl {

class OSLErrorScope;

class OSLError : public llvm::ErrorInfo<OSLError> {
public:

    static char ID;

    void log(llvm::raw_ostream&) const override;

    std::error_code convertToErrorCode() const override;

private:

    friend class OSLErrorScope;

    OSLError(int, const std::string&);

    int d_code = 0;
    std::string d_message;
};

} // End namespace llosl

#endif
