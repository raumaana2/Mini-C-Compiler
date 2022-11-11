#ifndef MCCOMP_HPP
#define MCCOMP_HPP


#include "llvm/ADT/APFloat.h"
#include "llvm/ADT/Optional.h"
#include "llvm/ADT/STLExtras.h"
#include "llvm/IR/BasicBlock.h"
#include "llvm/IR/Constants.h"
#include "llvm/IR/DerivedTypes.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/Instructions.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/LegacyPassManager.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Type.h"
#include "llvm/IR/Verifier.h"
#include "llvm/Support/FileSystem.h"
#include "llvm/Support/Host.h"
#include "llvm/MC/TargetRegistry.h"
#include "llvm/Support/TargetSelect.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Target/TargetMachine.h"
#include "llvm/Target/TargetOptions.h"
#include <algorithm>
#include <cassert>
#include <cctype>
#include <cstdio>
#include <cstdlib>
#include <iostream>
#include <map>
#include <memory>
#include <queue>
#include <string.h>
#include <string>
#include <system_error>
#include <utility>
#include <vector>

using namespace llvm;
using namespace llvm::sys;


// The lexer returns one of these for known things.
enum TOKEN_TYPE {

  IDENT = -1,        // [a-zA-Z_][a-zA-Z_0-9]*
  ASSIGN = int('='), // '='

  // delimiters
  LBRA = int('{'),  // left brace
  RBRA = int('}'),  // right brace
  LPAR = int('('),  // left parenthesis
  RPAR = int(')'),  // right parenthesis
  SC = int(';'),    // semicolon
  COMMA = int(','), // comma

  // types
  INT_TOK = -2,   // "int"
  VOID_TOK = -3,  // "void"
  FLOAT_TOK = -4, // "float"
  BOOL_TOK = -5,  // "bool"

  // keywords
  EXTERN = -6,  // "extern"
  IF = -7,      // "if"
  ELSE = -8,    // "else"
  WHILE = -9,   // "while"
  RETURN = -10, // "return"
  // TRUE   = -12,     // "true"
  // FALSE   = -13,     // "false"

  // literals
  INT_LIT = -14,   // [0-9]+
  FLOAT_LIT = -15, // [0-9]+.[0-9]+
  BOOL_LIT = -16,  // "true" or "false" key words

  // logical operators
  AND = -17, // "&&"
  OR = -18,  // "||"

  // operators
  PLUS = int('+'),    // addition or unary plus
  MINUS = int('-'),   // substraction or unary negative
  ASTERIX = int('*'), // multiplication
  DIV = int('/'),     // division
  MOD = int('%'),     // modular
  NOT = int('!'),     // unary negation

  // comparison operators
  EQ = -19,      // equal
  NE = -20,      // not equal
  LE = -21,      // less than or equal to
  LT = int('<'), // less than
  GE = -23,      // greater than or equal to
  GT = int('>'), // greater than

  // special tokens
  EOF_TOK = 0, // signal end of file

  // invalid
  INVALID = -100 // signal invalid token
};

// TOKEN struct is used to keep track of information about a token
struct TOKEN {
  int type = -100;
  std::string lexeme;
  int lineNo;
  int columnNo;
};







std::unique_ptr<ast_node> parser();
std::unique_ptr<ast_node> program();
void extern_list(std::vector<std::unique_ptr<ast_node>>& list);
void extern_list_prime(std::vector<std::unique_ptr<ast_node>>& list);
std::unique_ptr<ast_node> extern_();
void decl_list(std::vector<std::unique_ptr<ast_node>>& list);
void decl_list_prime(std::vector<std::unique_ptr<ast_node>>& list);
std::unique_ptr<ast_node> decl();
std::unique_ptr<var_decl_ast> var_decl();
std::unique_ptr<function_ast> fun_decl();
TOKEN var_type();
TOKEN type_spec();
std::vector<std::unique_ptr<ast_node>> params();
void param_list(std::vector<std::unique_ptr<ast_node>>& list);
void param_list_prime(std::vector<std::unique_ptr<ast_node>>& list);
std::unique_ptr<ast_node> param();
std::unique_ptr<ast_node> block();
void local_decls(std::vector<std::unique_ptr<ast_node>>& list);
void local_decls_prime(std::vector<std::unique_ptr<ast_node>>& list);
std::unique_ptr<ast_node> local_decl();
void stmt_list(std::vector<std::unique_ptr<ast_node>>& list);
void stmt_list_prime(std::vector<std::unique_ptr<ast_node>>& list);
std::unique_ptr<ast_node> stmt();
std::unique_ptr<ast_node> expr_stmt();
std::unique_ptr<ast_node> while_stmt();
std::unique_ptr<ast_node> if_stmt();
std::unique_ptr<ast_node> else_stmt();
std::unique_ptr<ast_node> return_stmt();
std::unique_ptr<ast_node> return_stmt_B();
std::unique_ptr<ast_node> expr();
std::unique_ptr<ast_node> or_val();
std::unique_ptr<ast_node> or_val_prime(std::unique_ptr<ast_node> lhs);
std::unique_ptr<ast_node> and_val();
std::unique_ptr<ast_node> and_val_prime(std::unique_ptr<ast_node> lhs);
std::unique_ptr<ast_node> eq_val();
std::unique_ptr<ast_node> eq_val_prime(std::unique_ptr<ast_node> lhs);
std::unique_ptr<ast_node> comp_val();
std::unique_ptr<ast_node> comp_val_prime(std::unique_ptr<ast_node> lhs);
std::unique_ptr<ast_node> add_val();
std::unique_ptr<ast_node> add_val_prime(std::unique_ptr<ast_node> lhs);
std::unique_ptr<ast_node> mul_val();
std::unique_ptr<ast_node> mul_val_prime(std::unique_ptr<ast_node> lhs);
std::unique_ptr<ast_node> unary();
std::unique_ptr<ast_node> identifiers();
std::unique_ptr<ast_node> identifiers_B();
std::vector<std::unique_ptr<ast_node>> args();
void arg_list(std::vector<std::unique_ptr<ast_node>>& list);
void arg_list_prime(std::vector<std::unique_ptr<ast_node>>& list);



#endif