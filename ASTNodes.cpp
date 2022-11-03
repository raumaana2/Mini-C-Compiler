//===----------------------------------------------------------------------===//
// AST nodes
//===----------------------------------------------------------------------===//

/// ASTnode - Base class for all AST nodes.
class ASTnode {
public:
  virtual ~ASTnode() {}
  virtual Value *codegen() = 0;
  virtual std::string to_string() const {};
};

/// IntASTnode - Class for integer literals like 1, 2, 10,
class IntASTnode : public ASTnode {
  int Val;
  TOKEN Tok;
  std::string Name;

public:
  IntASTnode(TOKEN tok, int val) : Val(val), Tok(tok) {}
  virtual Value *codegen() override;
  // virtual std::string to_string() const override {
  // return a sting representation of this AST node
  //};
};

/// FloatASTnode - Class for float literals like 1.0, 2.5, 10.000001,
class FloatASTnode : public ASTnode {
  float Val;
  TOKEN Tok;
  std::string Name;

public:
  FloatASTnode(TOKEN tok, float val) : Val(val), Tok(tok) {}
  virtual Value *codegen() override;
  // virtual std::string to_string() const override {
  // return a sting representation of this AST node
  //};
};


/// BoolASTnode - Class for boolean literals like true, false
class BoolASTnode : public ASTnode {
  bool Val;
  TOKEN Tok;
  std::string Name;

public:
  BoolASTnode(TOKEN tok, bool val) : Val(val), Tok(tok) {}
  virtual Value *codegen() override;
  // virtual std::string to_string() const override {
  // return a sting representation of this AST node
  //};
};



class BinaryExprAST : public ASTnode {
    TOKEN Op;
    std::unique_ptr<ASTnode> LHS, RHS;


public:
    BinaryExprAST(TOKEN op, std::unique_ptr<ASTnode> LHS, 
        std::unique_ptr<ASTnode> RHS) : Op(op), LHS(std::move(LHS)),
    RHS(std::move(RHS)) {}

};



class CallExprAST : public ASTnode {
    std::string Callee;
    std::vector<std::unique_ptr<ASTnode>> Args;

public:
    CallExprAST(const std::string &callee,
        std::vector<std::unique_ptr<ASTnode>> args) :
    Callee(callee), Args(std::move(Args)) {}
};


class PrototypeAST {
    std::string Name;
    std::vector<std:string> Args;

public:
    PrototypeAST(const std::string &name, std::vector<std:string> Args) 
    : Name(name), Args(std::move(Args)) {}
};

class FunctionAST {
    std::unique_ptr<PrototypeAST>;
    std::unique_ptr<ExprAST> Body;

public:
    FunctionAST(std::unique_ptr<PrototypeAST> proto, 
        std::unique_ptr<ASTnode> body) : Proto(std::move(proto)), Body(std::move(body)) {}

}





class IfAST {
    
}


