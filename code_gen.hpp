#ifndef CODE_GEN_HPP
#define CODE_GEN_HPP

#include "common.hpp"

using namespace llvm;
using namespace llvm::sys;

//===----------------------------------------------------------------------===//
// Code Generation
//===----------------------------------------------------------------------===//

extern LLVMContext the_context;
extern IRBuilder<> Builder(the_context);
extern std::unique_ptr<Module> the_module;

#endif
