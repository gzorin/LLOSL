LLOSL - Generate LLVM IR from shaders written in the OpenShadingLanguage
========================================================================

This is a library that will generate LLVM IR from '.oso' files produced
by the [OpenShadingLanguage]() compiler, with the objective of later
executing them on a GPU.

This library links with the upstream OpenShadingLanguage libraries, but
it does not use the LLVM IR generation of the `liboslexec` library, since
that IR is usually discarded after being JIT'd, and it was difficult to
adapt it to run on a GPU.

This library has been used to generate shaders for Apple's Metal API.
It is a work in progress - many features of the language itself haven't
been implemented, and this repo currently lacks sample code.