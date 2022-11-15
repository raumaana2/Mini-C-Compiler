#include "ast_node.hpp"

std::string ProgramAST::to_string(int depth) const {
  std::string whitespace(depth, ' ');
  std::string list_elements = "";
  int cur_depth = depth;
  for (int i = 0; i < ExternList.size(); i++) {
    std::string element = (ExternList[i]) ? ExternList[i]->to_string(cur_depth) : "null";
    (i < ExternList.size() - 1) ? list_elements += element + "\n"
                           : list_elements += element;
  }
  list_elements += "\n";
  for (int i = 0; i < DeclList.size(); i++) {
    std::string element = (DeclList[i]) ? DeclList[i]->to_string(cur_depth) : "null";
    (i < DeclList.size() - 1) ? list_elements += element + "\n"
                           : list_elements += element;
  }
  return list_elements;
}

std::string BlockAST::to_string(int depth) const {
  std::string whitespace(depth, ' ');
  std::string list_elements = "";
  int cur_depth = depth;
  for (int i = 0; i < LocalDecls.size(); i++) {
    std::string element = (LocalDecls[i]) ? LocalDecls[i]->to_string(cur_depth) : "null";
    (i < LocalDecls.size() - 1) ? list_elements += element + "\n"
                           : list_elements += element;
  }
  list_elements += "\n";
  for (int i = 0; i < StmtList.size(); i++) {
    std::string element = (StmtList[i]) ? StmtList[i]->to_string(cur_depth) : "null";
    (i < StmtList.size() - 1) ? list_elements += element + "\n"
                           : list_elements += element;
  }
  return list_elements;
}


std::string LiteralASTNode::to_string(int depth) const {
  // return a sting representation of this AST node
  std::string whitespace(depth, ' ');
  return whitespace + Tok.lexeme;
}


std::string BinaryExprAST::to_string(int depth) const {
  // return a sting representation of this AST node

  std::string whitespace(depth, ' ');
  std::string left = (LHS) ? LHS->to_string(depth + 1) : "null";
  std::string right = (RHS) ? RHS->to_string(depth + 1) : "null";
  return whitespace + "Op: " + Op.lexeme + "\n" + whitespace + left + "\n" +
         whitespace + right;
}

std::string UnaryExprAST::to_string(int depth) const {
  // return a sting representation of this AST node
  std::string whitespace(depth, ' ');
  depth += 1;
  std::string expression = (Expr) ? Expr->to_string(depth + 1) : "null";
  return whitespace + "Op: " + Op.lexeme + "\n" + expression;
}

std::string CallExprAST::to_string(int depth) const {
  std::string arguments = "";
  for (int i = 0; i < Args.size(); i++) {
    std::string element = (Args[i]) ? Args[i]->to_string(depth + 1) : "null";
    (i < Args.size() - 1) ? arguments += element + ", " : arguments += element;
  }
  std::string whitespace(depth, ' ');
  return whitespace + Callee.lexeme + "(" + arguments + ")";
}

std::string PrototypeAST::to_string(int depth) const {
  std::string arguments = "";
  for (int i = 0; i < Args.size(); i++) {
    std::string element = (Args[i]) ? Args[i]->to_string(0) : "null";
    (i < Args.size() - 1) ? arguments += element + ", " : arguments += element;
  }
  std::string whitespace(depth, ' ');
  return whitespace + Type.lexeme + " " + Name.lexeme + "(" + arguments + ")";
}

std::string FunctionAST::to_string(int depth) const {
  // return a sting representation of this AST node
  std::string whitespace(depth, ' ');
  std::string prototype = (Proto) ? Proto->to_string(0) : "null";
  std::string functionbody = (Body) ? Body->to_string(depth + 1) : "null";
  return whitespace + prototype + "\n" + functionbody;
}

std::string IfAST::to_string(int depth) const {
  // return a sting representation of this AST node
  std::string con = (Condition) ? Condition->to_string(0) : "null";
  std::string ifBody = (IfBody) ? IfBody->to_string(depth + 1) : "null";
  std::string elseBody = (ElseBody) ? ElseBody->to_string(depth + 1) : "null";
  std::string whitespace(depth, ' ');
  return whitespace + "if (" + con + ")\n " + ifBody + "\n" + whitespace +
         "else \n" + whitespace + elseBody;
}

std::string WhileAST::to_string(int depth) const {
  // return a sting representation of this AST node
  std::string con = (Condition) ? Condition->to_string(0) : "null";
  std::string whilebody = (Body) ? Body->to_string(depth + 1) : "null";
  std::string whitespace(depth, ' ');
  return whitespace + "while (" + con + ") \n" + whilebody;
}


std::string ReturnAST::to_string(int depth) const {
  // return a sting representation of this AST node
  std::string returnbody = (Body) ? Body->to_string(0) : "null";
  std::string whitespace(depth, ' ');
  return whitespace + "return " + returnbody;
}


std::string VarDeclAST::to_string(int depth) const {
  // return a sting representation of this AST node
  std::string whitespace(depth, ' ');
  if (Type.lexeme == "void") {
    return whitespace + Type.lexeme;
  }
  return whitespace + Type.lexeme + " " + Name.lexeme;
}


std::string VarAssignAST::to_string(int depth) const {
  // return a sting representation of this AST node
  std::string expression = (Expr) ? Expr->to_string(depth + 1) : "null";
  std::string whitespace(depth, ' ');
  return whitespace + Name.lexeme + " assigned\n" + expression;
}
