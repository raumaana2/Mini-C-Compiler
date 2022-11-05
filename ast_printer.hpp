#ifndef AST_PRINTER_H
#define AST_PRINTER_H

#include "common.hpp"

inline llvm::raw_ostream &operator<<(llvm::raw_ostream &os,
                                     const ast_node &ast);


#endif