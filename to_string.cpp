#include "ast_node.hpp"


/**
 * to_string methods that horizontally print an AST tree constructed by the parser
 * works by passing around a prefix that indents and adds branches
 */

//program ast to string
std::string ProgramAST::to_string(std::string prefix) const {
  std::string string_builder = "extern\n";
  //externs 
  for (int i = 0; i < ExternList.size(); i++) {
    if (i < ExternList.size() - 1) { //left
      std::string element = "├──" + ((ExternList[i]) ? ExternList[i]->to_string(prefix + "│   ") : "null");
      string_builder += element + "\n";
    } else {  //right
      std::string element = "└──" + ((ExternList[i]) ? ExternList[i]->to_string(prefix + "    ") : "null");
      string_builder += element + "\n";
    }
  }
  string_builder += "declarations\n";
  //declarations
  for (int i = 0; i < DeclList.size(); i++) {
    if (i < DeclList.size() - 1) { //left
      std::string element = "├──" + ((DeclList[i]) ? DeclList[i]->to_string(prefix + "│   ") : "null");
      string_builder += element + "\n";

    } else {  //right
      std::string element = "└──" + ((DeclList[i]) ? DeclList[i]->to_string(prefix + "    ") : "null");
      string_builder += element;

    }
  }
  return string_builder;
}

//block ast to string
std::string BlockAST::to_string(std::string prefix) const {
  std::string string_builder = "local declarations\n";
  //local declarations
  for (int i = 0; i < LocalDecls.size(); i++) {
    if (i < LocalDecls.size() - 1) { //left
      std::string element = prefix + "├──" + ((LocalDecls[i]) ? LocalDecls[i]->to_string(prefix + "│   ") : "null");
      string_builder += element + "\n";
    } else {  //right
      std::string element = prefix + "└──" + ((LocalDecls[i]) ? LocalDecls[i]->to_string(prefix + "    ") : "null");
      string_builder += element + "\n";
    }
  }
  string_builder += prefix + "statements\n";
  //statements
  for (int i = 0; i < StmtList.size(); i++) {
    if (i < StmtList.size() - 1) { //left
      std::string element = prefix + "├──" + ((StmtList[i]) ? StmtList[i]->to_string(prefix + "│   ") : "null");
      string_builder += element + "\n";

    } else {  //right
      std::string element = prefix + "└──" + ((StmtList[i]) ? StmtList[i]->to_string(prefix + "    ") : "null");
      string_builder += element;

    }
  }
  return string_builder;
}

//binary expression to string
std::string BinaryExprAST::to_string(std::string prefix) const {
  std::string string_builder = "Op:" + Op.lexeme + "\n";
  std::string left = prefix + "├──" + ((LHS) ? LHS->to_string(prefix + "│   ") : "null");
  std::string right = prefix + "└──" + ((RHS) ? RHS->to_string(prefix + "    ") : "null");
  

  string_builder += left + "\n" + right;

  return string_builder;
}

//literal ast to string
std::string LiteralASTNode::to_string(std::string prefix) const {
  std::string string_builder = "literal: " + Tok.lexeme;
  return string_builder;
}


//unary expression to string
std::string UnaryExprAST::to_string(std::string prefix) const {
  std::string string_builder = "unary "+ Op.lexeme + "\n";

  std::string expression = prefix + "└──" + ((Expr) ? Expr->to_string(prefix + "    ") : "null");

  string_builder += expression;
  return string_builder;
}

//function call to string
std::string CallExprAST::to_string(std::string prefix) const {
  std::string string_builder = "";
  std::string arguments = "";
  //call arguments to string
  for (int i = 0; i < Args.size(); i++) {
    if (i < Args.size() - 1) { //left
      std::string element = prefix + "├──" + ((Args[i]) ? Args[i]->to_string(prefix + "│   ") : "null");
      arguments += element + "\n";

    } else {  //right
      std::string element = prefix + "└──" + ((Args[i]) ? Args[i]->to_string(prefix + "    ") : "null");
      arguments += element;

    }
  }
  string_builder += "call: " + Callee.lexeme + "\n" + arguments;
  return string_builder;
}

//function prototype to string
std::string PrototypeAST::to_string(std::string prefix) const {
  std::string string_builder = Type.lexeme + " " + Name.lexeme;
  std::string arguments = "(";
  //parameters to string
  for (int i = 0; i < Args.size(); i++) {
    std::string element = ((Args[i]) ? Args[i]->to_string("") : "null");
    if (i < Args.size() - 1) {
      arguments += element + ", ";
    } else {
      arguments += element + ")";      
    }
  }

  string_builder += arguments;
  return string_builder;
}

//function ast to string
std::string FunctionAST::to_string(std::string prefix) const {
  std::string string_builder = "";
  std::string prototype = ((Proto) ? Proto->to_string(prefix + "│   ") : "null");
  std::string functionbody = prefix + "└──" + ((Body) ? Body->to_string(prefix + "    ") : "null");
  string_builder += prototype + "\n" + functionbody;
  return string_builder;
}

//if else ast to string
std::string IfAST::to_string(std::string prefix) const {
  std::string string_builder = "";
  std::string con = prefix + "├──" + ((Condition) ? Condition->to_string(prefix + "│   ") : "null");
  std::string ifbody = prefix + "├──" + ((IfBody) ? IfBody->to_string(prefix + "│   ") : "null");
  std::string elsebody = prefix + "└──" + ((ElseBody) ? ElseBody->to_string(prefix + "    ") : "null");

  string_builder += "if\n" + con + "\n" + ifbody + "\n" + prefix + "else\n" + elsebody;

  return string_builder;
}

//while ast to string
std::string WhileAST::to_string(std::string prefix) const {
  std::string string_builder = "";
  std::string condition = prefix + "├──" + ((Condition) ? Condition->to_string(prefix + "│   "): "null");
  std::string whilebody = prefix + "└──" + ((Body) ? Body->to_string(prefix + "    "): "null");

  string_builder += "while\n" + condition + "\n" + whilebody;
  return string_builder;
  
}

//return ast to string
std::string ReturnAST::to_string(std::string prefix) const {
  std::string string_builder = "";

  std::string body = prefix + "└──" + ((Body) ? Body->to_string(prefix + "    ") : "null" );

  string_builder += "return\n" + body;
  
  return string_builder;
}

//variable declaration to string
std::string VarDeclAST::to_string(std::string prefix) const {
  std::string string_builder = Type.lexeme + " " + Name.lexeme;

  return string_builder;
}

// variable assignment to string
std::string VarAssignAST::to_string(std::string prefix) const {
  std::string string_builder = "";

  std::string expression = prefix + "└──" + ((Expr) ? Expr->to_string(prefix + "    ") : "null");

  string_builder += Name.lexeme + "\n" + prefix + "=\n" + expression;

  return string_builder;
}
