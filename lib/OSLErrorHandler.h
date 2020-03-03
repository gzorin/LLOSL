//-*-C++-*-
#ifndef LLOSL_OSLERRORHANDLER_H
#define LLOSL_OSLERRORHANDLER_H

#include <llosl/OSLError.h>

#include <OSL/oslexec.h>

#include <stack>

namespace llosl {

class OSLErrorHandler;

class OSLErrorScope {
public:
    OSLErrorScope(OSLErrorScope &&);
    ~OSLErrorScope();

    OSLErrorScope()                      = delete;
    OSLErrorScope(const OSLErrorScope &) = delete;

    OSLErrorScope &operator=(const OSLErrorScope &) = delete;
    OSLErrorScope &operator                         =(OSLErrorScope &&);

    llvm::Error takeError();

private:
    friend class OSLErrorHandler;

    OSLErrorScope(OSLErrorHandler *);

    OSLErrorHandler *d_context = nullptr;
    bool             d_checked = false;
    int              d_code    = 0;
    std::string      d_message;
};

class OSLErrorHandler : public OSL::ErrorHandler {
public:
    OSLErrorScope enter();

    // OSL::ErrorHandler overrides:
    void operator()(int, const std::string &);

private:
    friend class OSLErrorScope;

    std::stack<OSLErrorScope *> d_scopes;
};

} // namespace llosl

#endif
