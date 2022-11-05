#ifndef AST_HPP
#define AST_HPP

#include "common.h"

//===----------------------------------------------------------------------===//
// AST nodes
//===----------------------------------------------------------------------===//

/// ast_node - Base class for all AST nodes.
class ast_node {
public:
  virtual ~ast_node() {}
  virtual Value *codegen() = 0;
  virtual std::string to_string() const {};
};

/// int_ast_node - Class for integer literals like 1, 2, 10,
class literal_ast_node : public ast_node {
  TOKEN Tok;
  

public:
  literal_ast_node(TOKEN tok) : Tok(tok) {}
  virtual Value *codegen() override;
  // virtual std::string to_string() const override {
  // return a sting representation of this AST node
  //};
};


// /// int_ast_node - Class for integer literals like 1, 2, 10,
// class int_ast_node : public ast_node {
//   int Val;
//   TOKEN Tok;
//   std::string Name;

// public:
//   int_ast_node(TOKEN tok, int val) : Val(val), Tok(tok) {}
//   virtual Value *codegen() override;
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
//   virtual Value *codegen() override;
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
//   virtual Value *codegen() override;
//   // virtual std::string to_string() const override {
//   // return a sting representation of this AST node
//   //};
// };


/// void_ast_node - Class for boolean literals like true, false
class void_ast_node : public ast_node {
  TOKEN Tok;

public:
  void_ast_node(TOKEN tok) : Tok(tok) {}
  virtual Value *codegen() override;
  // virtual std::string to_string() const override {
  // return a sting representation of this AST node
  //};
};

// Class for binary expressions
class binary_expr_ast : public ast_node {
    TOKEN Op;
    std::unique_ptr<ast_node> LHS, RHS;


public:
    binary_expr_ast(TOKEN op, std::unique_ptr<ast_node> LHS, 
        std::unique_ptr<ast_node> RHS) : Op(op), LHS(std::move(LHS)),
    RHS(std::move(RHS)) {}

};

// class for unary expressions
class unary_expr_ast : public ast_node {
  TOKEN Op;
  std::unique_ptr<ast_node> Expr;

public:
  unary_expr_ast(TOKEN op, std::unique_ptr<ast_node> expr) : Op(op), Expr(std::move(expr)) {}
};

// class for function calls
class call_expr_ast : public ast_node {
    TOKEN Callee;
    std::vector<std::unique_ptr<ast_node>> Args;

public:
    call_expr_ast(TOKEN callee,
        std::vector<std::unique_ptr<ast_node>> args) :
    Callee(callee), Args(std::move(Args)) {}
};


//class for function signature
class prototype_ast : public ast_node {
    TOKEN Type;
    TOKEN Name;
    std::vector<std::unique_ptr<ast_node>> Args;

public:
    prototype_ast(TOKEN type, TOKEN name, std::vector<std::unique_ptr<ast_node>> args) 
    : Type(type), Name(name), Args(std::move(args)) {}
};


//class for function signature and body
class function_ast : public ast_node  {
    std::unique_ptr<prototype_ast> Proto;
    std::unique_ptr<ast_node> Body;

public:
    function_ast(std::unique_ptr<prototype_ast> proto, 
        std::unique_ptr<ast_node> body) : Proto(std::move(proto)), Body(std::move(body)) {}

};


// class for if statement structure
class if_ast : public ast_node {
    std::unique_ptr<ast_node> Condition;
    std::unique_ptr<ast_node> If_body;
    std::unique_ptr<ast_node> Else_body;
    
public:
  if_ast(std::unique_ptr<ast_node> condition, std::unique_ptr<ast_node> if_body, std::unique_ptr<ast_node> else_body) :
    Condition(std::move(condition)), If_body(std::move(if_body)), Else_body(std::move(else_body)) {}
};

// class for while statement structure
class while_ast : public ast_node {
    std::unique_ptr<ast_node> Condition;
    std::unique_ptr<ast_node> Body;
    
public:
  while_ast(std::unique_ptr<ast_node> condition, std::unique_ptr<ast_node> body) :
    Condition(std::move(condition)), Body(std::move(body)) {}
};

//class for return statement structure
class return_ast : public ast_node {
  std::unique_ptr<ast_node> Body;

public:
  return_ast(std::unique_ptr<ast_node> body) : Body(std::move(body)) {}
};

//class for variable declaration structure
class var_decl_ast : public ast_node {
  TOKEN Type;
  TOKEN Name;

public:
  var_decl_ast(TOKEN type, TOKEN name) : Type(type), Name(name)  {}
};

// class for variable assignment structure
class var_assign_ast : public ast_node {
  TOKEN Name;
  std::unique_ptr<ast_node> Expr;

public:
  var_assign_ast(TOKEN name, std::unique_ptr<ast_node> expr) : Name(name), Expr(std::move(expr)) {

  }
};


class scope_ast : public ast_node {
  std::vector<std::unique_ptr<ast_node>> Lists;

public:
  scope_ast(std::vector<std::unique_ptr<ast_node>> lists) : Lists(std::move(lists)) {}
};

class identifier_ast : public ast_node {
  TOKEN Identifier;

public: 
  identifier_ast(TOKEN identifier) : Identifier(identifier) {}
};


#endif