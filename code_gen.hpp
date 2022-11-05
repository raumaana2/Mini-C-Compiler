#ifndef CODE_GEN_HPP
#define CODE_GEN_HPP

#include "common.hpp"


//===----------------------------------------------------------------------===//
// Code Generation
//===----------------------------------------------------------------------===//

extern LLVMContext TheContext;
extern IRBuilder<> Builder(TheContext);
extern std::unique_ptr<Module> TheModule;

#endif
