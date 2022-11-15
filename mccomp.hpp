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

//===----------------------------------------------------------------------===//
// AST nodes
//===----------------------------------------------------------------------===//

/// ASTNode - Base class for all AST nodes.
class ASTNode {
public:
  virtual ~ASTNode() {}
  virtual Value *codegen() = 0;
  virtual std::string to_string(int depth) const { return ""; };
};

class ProgramAST : public ASTNode {
  std::vector<std::unique_ptr<ASTNode>> ExternList;
  std::vector<std::unique_ptr<ASTNode>> DeclList;

public:
  ProgramAST(std::vector<std::unique_ptr<ASTNode>> externList,
           std::vector<std::unique_ptr<ASTNode>> declList)
      : ExternList(std::move(externList)), DeclList(std::move(declList)) {}

  Value *codegen() override;
  std::string to_string(int depth) const override; 
};

// class for variable declaration structure
class VarDeclAST : public ASTNode {

public:
  TOKEN Type;
  
  TOKEN Name;

  VarDeclAST(TOKEN type, TOKEN name) : Type(type), Name(name) {}

  Value* codegen() override;

  std::string to_string(int depth) const override;

  
  
};


class BlockAST : public ASTNode {
  std::vector<std::unique_ptr<ASTNode>> LocalDecls;
  std::vector<std::unique_ptr<ASTNode>> StmtList;

public:
  BlockAST(std::vector<std::unique_ptr<ASTNode>> localDecls,
           std::vector<std::unique_ptr<ASTNode>> stmtList)
      : LocalDecls(std::move(localDecls)), StmtList(std::move(stmtList)) {}

  Value *codegen() override;
  std::string to_string(int depth) const override;
};

/// LiteralASTNode - class to store integer, float or boolean literals or
/// indentifiers
class LiteralASTNode : public ASTNode {
  TOKEN Tok;

public:
  LiteralASTNode(TOKEN tok) : Tok(tok) {}

  Value *codegen() override;
  std::string to_string(int depth) const override;
  
};


// Class for binary expressions
class BinaryExprAST : public ASTNode {
  TOKEN Op;
  std::unique_ptr<ASTNode> LHS, RHS;

public:
  BinaryExprAST(TOKEN op, std::unique_ptr<ASTNode> LHS,
                std::unique_ptr<ASTNode> RHS)
      : Op(op), LHS(std::move(LHS)), RHS(std::move(RHS)) {}

  Value *codegen() override;

  std::string to_string(int depth) const override;
  std::string getOp() {
    return Op.lexeme;
  }
};

// class for unary expressions
class UnaryExprAST : public ASTNode {
  TOKEN Op;
  std::unique_ptr<ASTNode> Expr;

public:
  UnaryExprAST(TOKEN op, std::unique_ptr<ASTNode> expr)
      : Op(op), Expr(std::move(expr)) {}

  Value *codegen() override;

  std::string to_string(int depth) const override;

  std::string getOp() {
    return Op.lexeme;
  }
};

// class for function calls
class CallExprAST : public ASTNode {
  TOKEN Callee;
  std::vector<std::unique_ptr<ASTNode>> Args;

public:
  CallExprAST(TOKEN callee, std::vector<std::unique_ptr<ASTNode>> args)
      : Callee(callee), Args(std::move(args)) {}

  Value *codegen() override;

  std::string to_string(int depth) const override;

  std::string getCallee() {
    return Callee.lexeme;
  }
};

// class for function signature (used for for both the function definition and
// externs)
class PrototypeAST : public ASTNode {
  TOKEN Type;
  TOKEN Name;
  std::vector<std::unique_ptr<VarDeclAST>> Args;

public:
  PrototypeAST(TOKEN type, TOKEN name,
               std::vector<std::unique_ptr<VarDeclAST>> args)
      : Type(type), Name(name), Args(std::move(args)) {}

  Function *codegen() override;

  std::string to_string(int depth) const override;

  std::string getName() {
    return Name.lexeme;
  }

  TOKEN getProtoType() {
    return Type;
  }
};

// class for function signature and body
class FunctionAST : public ASTNode {
  std::unique_ptr<PrototypeAST> Proto;
  std::unique_ptr<BlockAST> Body;

public:
  FunctionAST(std::unique_ptr<PrototypeAST> proto,
              std::unique_ptr<BlockAST> body)
      : Proto(std::move(proto)), Body(std::move(body)) {}
  Function *codegen() override;
  std::string to_string(int depth) const override;

};

// class for if statement structure
class IfAST : public ASTNode {
  std::unique_ptr<ASTNode> Condition;
  std::unique_ptr<ASTNode> IfBody;
  std::unique_ptr<ASTNode> ElseBody;

public:
  IfAST(std::unique_ptr<ASTNode> condition, std::unique_ptr<ASTNode> ifBody,
        std::unique_ptr<ASTNode> elseBody)
      : Condition(std::move(condition)), IfBody(std::move(ifBody)),
        ElseBody(std::move(elseBody)) {}

  Value *codegen() override;

  std::string to_string(int depth) const override;
};

// class for while statement structure
class WhileAST : public ASTNode {
  std::unique_ptr<ASTNode> Condition;
  std::unique_ptr<ASTNode> Body;

public:
  WhileAST(std::unique_ptr<ASTNode> condition, std::unique_ptr<ASTNode> body)
      : Condition(std::move(condition)), Body(std::move(body)) {}

  Value *codegen() override;

  std::string to_string(int depth) const override;
};

// class for return statement structure
class ReturnAST : public ASTNode {
  std::unique_ptr<ASTNode> Body;

public:
  ReturnAST(std::unique_ptr<ASTNode> body) : Body(std::move(body)) {}

  Value* codegen() override; 

  std::string to_string(int depth) const override;
};


// class for variable assignment structure
class VarAssignAST : public ASTNode {
  TOKEN Name;
  std::unique_ptr<ASTNode> Expr;

public:
  VarAssignAST(TOKEN name, std::unique_ptr<ASTNode> expr)
      : Name(name), Expr(std::move(expr)) {}

  Value* codegen() override;

  std::string to_string(int depth) const override;
};



std::unique_ptr<ASTNode> parser();
std::unique_ptr<ASTNode> program();
void extern_list(std::vector<std::unique_ptr<ASTNode>> &list);
void extern_list_prime(std::vector<std::unique_ptr<ASTNode>> &list);
std::unique_ptr<ASTNode> extern_();
void decl_list(std::vector<std::unique_ptr<ASTNode>> &list);
void decl_list_prime(std::vector<std::unique_ptr<ASTNode>> &list);
std::unique_ptr<ASTNode> decl();
std::unique_ptr<VarDeclAST> var_decl();
std::unique_ptr<FunctionAST> fun_decl();
TOKEN var_type();
TOKEN type_spec();
std::vector<std::unique_ptr<VarDeclAST>> params();
void param_list(std::vector<std::unique_ptr<VarDeclAST>> &list);
void param_list_prime(std::vector<std::unique_ptr<VarDeclAST>> &list);
std::unique_ptr<VarDeclAST> param();
std::unique_ptr<BlockAST> block();
void local_decls(std::vector<std::unique_ptr<ASTNode>> &list);
void local_decls_prime(std::vector<std::unique_ptr<ASTNode>> &list);
std::unique_ptr<ASTNode> local_decl();
void stmt_list(std::vector<std::unique_ptr<ASTNode>> &list);
void stmt_list_prime(std::vector<std::unique_ptr<ASTNode>> &list);
std::unique_ptr<ASTNode> stmt();
std::unique_ptr<ASTNode> expr_stmt();
std::unique_ptr<ASTNode> while_stmt();
std::unique_ptr<ASTNode> if_stmt();
std::unique_ptr<ASTNode> else_stmt();
std::unique_ptr<ASTNode> return_stmt();
std::unique_ptr<ASTNode> return_stmt_B();
std::unique_ptr<ASTNode> expr();
std::unique_ptr<ASTNode> or_val();
std::unique_ptr<ASTNode> or_val_prime(std::unique_ptr<ASTNode> lhs);
std::unique_ptr<ASTNode> and_val();
std::unique_ptr<ASTNode> and_val_prime(std::unique_ptr<ASTNode> lhs);
std::unique_ptr<ASTNode> eq_val();
std::unique_ptr<ASTNode> eq_val_prime(std::unique_ptr<ASTNode> lhs);
std::unique_ptr<ASTNode> comp_val();
std::unique_ptr<ASTNode> comp_val_prime(std::unique_ptr<ASTNode> lhs);
std::unique_ptr<ASTNode> add_val();
std::unique_ptr<ASTNode> add_val_prime(std::unique_ptr<ASTNode> lhs);
std::unique_ptr<ASTNode> mul_val();
std::unique_ptr<ASTNode> mul_val_prime(std::unique_ptr<ASTNode> lhs);
std::unique_ptr<ASTNode> unary();
std::unique_ptr<ASTNode> identifiers();
std::unique_ptr<ASTNode> identifiers_B();
std::vector<std::unique_ptr<ASTNode>> args();
void arg_list(std::vector<std::unique_ptr<ASTNode>> &list);
void arg_list_prime(std::vector<std::unique_ptr<ASTNode>> &list);








#endif