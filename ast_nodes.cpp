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
class int_ast_node : public ast_node {
  int Val;
  TOKEN Tok;
  std::string Name;

public:
  int_ast_node(TOKEN tok, int val) : Val(val), Tok(tok) {}
  virtual Value *codegen() override;
  // virtual std::string to_string() const override {
  // return a sting representation of this AST node
  //};
};

/// float_ast_node - Class for float literals like 1.0, 2.5, 10.000001,
class float_ast_node : public ast_node {
  float Val;
  TOKEN Tok;
  std::string Name;

public:
  float_ast_node(TOKEN tok, float val) : Val(val), Tok(tok) {}
  virtual Value *codegen() override;
  // virtual std::string to_string() const override {
  // return a sting representation of this AST node
  //};
};


/// bool_ast_node - Class for boolean literals like true, false
class bool_ast_node : public ast_node {
  bool Val;
  TOKEN Tok;
  std::string Name;

public:
  bool_ast_node(TOKEN tok, bool val) : Val(val), Tok(tok) {}
  virtual Value *codegen() override;
  // virtual std::string to_string() const override {
  // return a sting representation of this AST node
  //};
};



class binary_expr_ast : public ast_node {
    TOKEN Op;
    std::unique_ptr<ast_node> LHS, RHS;


public:
    binary_expr_ast(TOKEN op, std::unique_ptr<ast_node> LHS, 
        std::unique_ptr<ast_node> RHS) : Op(op), LHS(std::move(LHS)),
    RHS(std::move(RHS)) {}

};



class call_expr_ast : public ast_node {
    std::string Callee;
    std::vector<std::unique_ptr<ast_node>> Args;

public:
    call_expr_ast(const std::string &callee,
        std::vector<std::unique_ptr<ast_node>> args) :
    Callee(callee), Args(std::move(Args)) {}
};


class prototype_ast {
    std::string Name;
    std::vector<std:string> Args;

public:
    prototype_ast(const std::string &name, std::vector<std:string> Args) 
    : Name(name), Args(std::move(Args)) {}
};

class function_ast {
    std::unique_ptr<prototype_ast>;
    std::unique_ptr<ExprAST> Body;

public:
    function_ast(std::unique_ptr<prototype_ast> proto, 
        std::unique_ptr<ast_node> body) : Proto(std::move(proto)), Body(std::move(body)) {}

}





class IfAST {
    
}

class 


