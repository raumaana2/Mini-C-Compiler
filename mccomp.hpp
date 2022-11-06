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
#include "boost/format.hpp"

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

/// ast_node - Base class for all AST nodes.
class ast_node {
public:
  virtual ~ast_node() {}
  // virtual Value *codegen() = 0;
  virtual std::string to_string() const {};
};

/// int_ast_node - Class for integer literals like 1, 2, 10,
class literal_ast_node : public ast_node {
  TOKEN Tok;
  

public:
  literal_ast_node(TOKEN tok) : Tok(tok) {}
  // virtual Value *codegen() override;
  virtual std::string to_string() const override {
  // return a sting representation of this AST node
    return Tok.lexeme;
  };
};


// /// int_ast_node - Class for integer literals like 1, 2, 10,
// class int_ast_node : public ast_node {
//   int Val;
//   TOKEN Tok;
//   std::string Name;

// public:
//   int_ast_node(TOKEN tok, int val) : Val(val), Tok(tok) {}
  // virtual Value *codegen() override;
//   // virtual std::string to_string() const override {
//   // return a sting representation of this AST node
//   //};
// };

// /// float_ast_node - Class for float literals like 1.0, 2.5, 10.000001,
// class float_ast_node : public ast_node {
//   float Val;
//   TOKEN Tok;
//   std::string Name;

// public:
//   float_ast_node(TOKEN tok, float val) : Val(val), Tok(tok) {}
  // virtual Value *codegen() override;
//   // virtual std::string to_string() const override {
//   // return a sting representation of this AST node
//   //};
// };


// /// bool_ast_node - Class for boolean literals like true, false
// class bool_ast_node : public ast_node {
//   bool Val;
//   TOKEN Tok;
//   std::string Name;

// public:
//   bool_ast_node(TOKEN tok, bool val) : Val(val), Tok(tok) {}
  // virtual Value *codegen() override;
//   // virtual std::string to_string() const override {
//   // return a sting representation of this AST node
//   //};
// };


/// void_ast_node - Class for boolean literals like true, false
class void_ast_node : public ast_node {
  TOKEN Tok;

public:
  void_ast_node(TOKEN tok) : Tok(tok) {}
  // virtual Value *codegen() override;
  // virtual std::string to_string() const override {
  // return a sting representation of this AST node
  //};
  virtual std::string to_string() const override {
  // return a sting representation of this AST node
    return Tok.lexeme;
  };
};

// Class for binary expressions
class binary_expr_ast : public ast_node {
    TOKEN Op;
    std::unique_ptr<ast_node> LHS, RHS;


public:
    binary_expr_ast(TOKEN op, std::unique_ptr<ast_node> LHS, 
        std::unique_ptr<ast_node> RHS) : Op(op), LHS(std::move(LHS)),
    RHS(std::move(RHS)) {}

    virtual std::string to_string() const override {
    // return a sting representation of this AST node
      boost::format nodes = boost::format("Op:%1\n\t%2\n\t%3") % Op.lexeme % LHS.to_string() % RHS.to_string();

      return nodes.str();
    };

};

// class for unary expressions
class unary_expr_ast : public ast_node {
  TOKEN Op;
  std::unique_ptr<ast_node> Expr;

public:
  unary_expr_ast(TOKEN op, std::unique_ptr<ast_node> expr) : Op(op), Expr(std::move(expr)) {}
  virtual std::string to_string() const override {
    // return a sting representation of this AST node
    boost::format nodes = boost::format("Op:%1%\n\t%2%") % Op.lexeme % Expr.to_string();

    return nodes.str();
  };
};

// class for function calls
class call_expr_ast : public ast_node {
    TOKEN Callee;
    std::vector<std::unique_ptr<ast_node>> Args;

public:
  call_expr_ast(TOKEN callee,std::vector<std::unique_ptr<ast_node>> args) :
    Callee(callee), Args(std::move(args)) {}
  virtual std::string to_string() const override {
    // return a sting representation of this AST node
    boost::format nodes = boost::format("function call: %1%() ");
  };
};


//class for function signature
class prototype_ast : public ast_node {
    TOKEN Type;
    TOKEN Name;
    std::vector<std::unique_ptr<ast_node>> Args;

public:
  prototype_ast(TOKEN type, TOKEN name, std::vector<std::unique_ptr<ast_node>> args) 
    : Type(type), Name(name), Args(std::move(args)) {}
  virtual std::string to_string() const override {
    // return a sting representation of this AST node

  };
};


//class for function signature and body
class function_ast : public ast_node  {
    std::unique_ptr<prototype_ast> Proto;
    std::unique_ptr<ast_node> Body;

public:
  function_ast(std::unique_ptr<prototype_ast> proto, 
    std::unique_ptr<ast_node> body) : Proto(std::move(proto)), Body(std::move(body)) {}
  virtual std::string to_string() const override {
    // return a sting representation of this AST node

  };

};


// class for if statement structure
class if_ast : public ast_node {
    std::unique_ptr<ast_node> Condition;
    std::unique_ptr<ast_node> If_body;
    std::unique_ptr<ast_node> Else_body;
    
public:
  if_ast(std::unique_ptr<ast_node> condition, std::unique_ptr<ast_node> if_body, std::unique_ptr<ast_node> else_body) :
    Condition(std::move(condition)), If_body(std::move(if_body)), Else_body(std::move(else_body)) {}
  
  virtual std::string to_string() const override {
    // return a sting representation of this AST node

  };
};

// class for while statement structure
class while_ast : public ast_node {
    std::unique_ptr<ast_node> Condition;
    std::unique_ptr<ast_node> Body;
    
public:
  while_ast(std::unique_ptr<ast_node> condition, std::unique_ptr<ast_node> body) :
    Condition(std::move(condition)), Body(std::move(body)) {}
  
  virtual std::string to_string() const override {
    // return a sting representation of this AST node

  };
};

//class for return statement structure
class return_ast : public ast_node {
  std::unique_ptr<ast_node> Body;

public:
  return_ast(std::unique_ptr<ast_node> body) : Body(std::move(body)) {}
  virtual std::string to_string() const override {
    // return a sting representation of this AST node

  };
};

//class for variable declaration structure
class var_decl_ast : public ast_node {
  TOKEN Type;
  TOKEN Name;

public:
  var_decl_ast(TOKEN type, TOKEN name) : Type(type), Name(name)  {}
  virtual std::string to_string() const override {
    // return a sting representation of this AST node

  };
};

// class for variable assignment structure
class var_assign_ast : public ast_node {
  TOKEN Name;
  std::unique_ptr<ast_node> Expr;

public:
  var_assign_ast(TOKEN name, std::unique_ptr<ast_node> expr) : Name(name), Expr(std::move(expr)) {

  }
  virtual std::string to_string() const override {
    // return a sting representation of this AST node

  };
};


class scope_ast : public ast_node {
  std::vector<std::unique_ptr<ast_node>> List_a;
  std::vector<std::unique_ptr<ast_node>> List_b;

public:
  scope_ast(std::vector<std::unique_ptr<ast_node>> list_a, std::vector<std::unique_ptr<ast_node>> list_b) : 
    List_a(std::move(list_a)), List_b(std::move(list_b)) {}
  virtual std::string to_string() const override {
    // return a sting representation of this AST node

  };
};

class identifier_ast : public ast_node {
  TOKEN Identifier;

public: 
  identifier_ast(TOKEN identifier) : Identifier(identifier) {}
  virtual std::string to_string() const override {
    // return a sting representation of this AST node

  };
};



void parser();
std::unique_ptr<ast_node> program();
void extern_list(std::vector<std::unique_ptr<ast_node>> list);
void extern_list_prime(std::vector<std::unique_ptr<ast_node>> list);
std::unique_ptr<ast_node> extern_();
void decl_list(std::vector<std::unique_ptr<ast_node>> list);
void decl_list_prime(std::vector<std::unique_ptr<ast_node>> list);
std::unique_ptr<ast_node> decl();
std::unique_ptr<var_decl_ast> var_decl();
std::unique_ptr<function_ast> fun_decl();
TOKEN var_type();
TOKEN type_spec();
std::vector<std::unique_ptr<ast_node>> params();
void param_list(std::vector<std::unique_ptr<ast_node>> list);
void param_list_prime(std::vector<std::unique_ptr<ast_node>> list);
std::unique_ptr<ast_node> param();
std::unique_ptr<ast_node> block();
void local_decls(std::vector<std::unique_ptr<ast_node>> list);
void local_decls_prime(std::vector<std::unique_ptr<ast_node>> list);
std::unique_ptr<ast_node> local_decl();
void stmt_list(std::vector<std::unique_ptr<ast_node>> list);
void stmt_list_prime(std::vector<std::unique_ptr<ast_node>> list);
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
void arg_list(std::vector<std::unique_ptr<ast_node>> list);
void arg_list_prime(std::vector<std::unique_ptr<ast_node>> list);



#endif