#ifndef AST_NODE_HPP
#define AST_NODE_HPP

#include "common.hpp"

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
/**
 * AST for Program production that contains extern list and decl list
 */
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

/**
 * AST for Variable declarations and contains attributes type and name of token type
 * 
 */
class VarDeclAST : public ASTNode {

public:
  TOKEN Type;
  
  TOKEN Name;

  VarDeclAST(TOKEN type, TOKEN name) : Type(type), Name(name) {}

  Value* codegen() override;

  std::string to_string(int depth) const override;

  
  
};

/**
 * AST for blocks and contains localdecls list and stmtlist
 * 
 */
class BlockAST : public ASTNode {
  std::vector<std::unique_ptr<ASTNode>> LocalDecls;
  std::vector<std::unique_ptr<ASTNode>> StmtList;
  TOKEN Tok;

public:
  BlockAST(TOKEN tok, std::vector<std::unique_ptr<ASTNode>> localDecls,
           std::vector<std::unique_ptr<ASTNode>> stmtList)
      : Tok(tok) ,LocalDecls(std::move(localDecls)), StmtList(std::move(stmtList)) {}

  Value *codegen() override;
  std::string to_string(int depth) const override;
};

/**
 * AST for literals such as integer literals, boolean literals, float literals and identifiers and 
 * has token attribute that stores the value as a lexeme
 */
class LiteralASTNode : public ASTNode {
  TOKEN Tok;

public:
  LiteralASTNode(TOKEN tok) : Tok(tok) {}

  Value *codegen() override;
  std::string to_string(int depth) const override;
  
};


/**
 * AST for binary expressions and has attributes operator, LHS and RHS
 */
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

/**
 * AST for unary expressions and has attributes operator and expression
 */
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

/**
 * AST for function calls and has attributes callee name and a list of arguments 
 */
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

/**
 * AST for function prototypes and has attributes type, name and list of arguments
 */
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

/**
 * AST for function declaration which has attributes function prototype and body
 */
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

/**
 * AST for if statments which has attributes condition, if body and else body
 */
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

/**
 * AST for while loops and has attributes condition and body
 */
class WhileAST : public ASTNode {
  std::unique_ptr<ASTNode> Condition;
  std::unique_ptr<ASTNode> Body;

public:
  WhileAST(std::unique_ptr<ASTNode> condition, std::unique_ptr<ASTNode> body)
      : Condition(std::move(condition)), Body(std::move(body)) {}

  Value *codegen() override;

  std::string to_string(int depth) const override;
};

/**
 * AST for return statements and has attributes body
 */
class ReturnAST : public ASTNode {
  std::unique_ptr<ASTNode> Body;
  TOKEN Tok;
public:
  ReturnAST(TOKEN tok, std::unique_ptr<ASTNode> body) : Tok(tok), Body(std::move(body)) {}

  Value* codegen() override; 

  std::string to_string(int depth) const override;
};


/**
 * AST for variable assignments and has attributes name and expression
 */
class VarAssignAST : public ASTNode {
  TOKEN Name;
  std::unique_ptr<ASTNode> Expr;

public:
  VarAssignAST(TOKEN name, std::unique_ptr<ASTNode> expr)
      : Name(name), Expr(std::move(expr)) {}

  Value* codegen() override;

  std::string to_string(int depth) const override;
};


#endif