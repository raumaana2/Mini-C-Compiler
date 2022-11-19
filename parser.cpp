#include "parser.hpp"
#include "common.hpp"
#include "error_reporting.hpp"

//===----------------------------------------------------------------------===//
// Parser
//===----------------------------------------------------------------------===//

/// CurTok/getNextToken - Provide a simple token buffer.  CurTok is the
/// current token the parser is looking at.  getNextToken reads another token
/// from the lexer and updates CurTok with its results.
TOKEN CurTok;
std::deque<TOKEN> tok_buffer;

TOKEN getNextToken() {

  if (tok_buffer.size() == 0)
    tok_buffer.push_back(gettok());

  TOKEN temp = tok_buffer.front();
  tok_buffer.pop_front();

  return CurTok = temp;
}

void putBackToken(TOKEN tok) { tok_buffer.push_front(tok); }

void match(int word, std::string symbol) {
  if (CurTok.type == word) {
    getNextToken();
  } else {
    TOKEN tok = CurTok;
    LogSymbolError(tok, symbol);
  }
}

//===----------------------------------------------------------------------===//
// Recursive Descent Parser - Function call for each production
//===----------------------------------------------------------------------===//

std::unique_ptr<ASTNode> parser() {
  auto program_scope = program();

  switch (CurTok.type) {
  case (EOF_TOK):
    return std::move(program_scope);
  default:
    TOKEN tok = CurTok;
    LogSyntaxError(tok, "Expected EOF");
  }
}

// program -> extern_list decl_list | decl_list
std::unique_ptr<ASTNode> program() {
  std::vector<std::unique_ptr<ASTNode>> extern_vector;
  std::vector<std::unique_ptr<ASTNode>> decl_vector;

  switch (CurTok.type) {
  case (EXTERN):
    extern_list(extern_vector);
  case (BOOL_TOK):
  case (FLOAT_TOK):
  case (INT_TOK):
  case (VOID_TOK): {
    decl_list(decl_vector);
    auto scope = std::make_unique<ProgramAST>(std::move(extern_vector),
                                            std::move(decl_vector));
    return std::move(scope);
  }
  
  }
  TOKEN tok = CurTok;
  LogSyntaxError(tok, "expected extern or bool or float or int or void");
}

// extern_list -> extern extern_list_prime
void extern_list(std::vector<std::unique_ptr<ASTNode>> &list) {
  auto _extern_ = extern_();
  list.push_back(std::move(_extern_));
  extern_list_prime(list);
}

// extern_list_prime -> extern extern_list_prime | epsilon
void extern_list_prime(std::vector<std::unique_ptr<ASTNode>> &list) {

  switch (CurTok.type) {
  case (INT_TOK):
  case (FLOAT_TOK):
  case (BOOL_TOK):
  case (VOID_TOK):
    return;
  case (EXTERN):
    auto _extern_ = extern_();
    list.push_back(std::move(_extern_));
    extern_list_prime(list);
    return;
  }
  TOKEN tok = CurTok;
  LogSyntaxError(tok, "expected extern or bool or float or int or void");
}

// extern -> "extern" type_spec IDENT "(" params ")" ";"
std::unique_ptr<ASTNode> extern_() {
  match(EXTERN, "extern");
  auto type = type_spec();

  TOKEN identifier = CurTok;
  match(IDENT, "an identifier"); // consume identifier

  match(LPAR, "("); // consumer (

  auto parameters = params();
  match(RPAR, ")");
  match(SC, ";");
  auto prototype =
      std::make_unique<PrototypeAST>(type, identifier, std::move(parameters));

  return std::move(prototype);
}

// decl_list -> decl decl_list_prime
void decl_list(std::vector<std::unique_ptr<ASTNode>> &list) {

  auto declaration = decl();
  list.push_back(std::move(declaration));
  decl_list_prime(list);
}

// decl_list -> decl decl_list_prime | epsilon
void decl_list_prime(std::vector<std::unique_ptr<ASTNode>> &list) {
  if (CurTok.type != EOF_TOK) {
    auto declaration = decl();
    list.push_back(std::move(declaration));
    decl_list_prime(list);
  }
}

// lookahead function used by decl to decide between var_decl and fun_decl
TOKEN peekll3() {
  TOKEN old = CurTok;

  TOKEN lookahead1 = getNextToken();
  TOKEN lookahead2 = getNextToken();

  TOKEN temp = lookahead2; // token to return for lookahead

  putBackToken(lookahead2);
  putBackToken(lookahead1);

  CurTok = old;

  return temp;
}

// decl -> var_decl | fun_decl
std::unique_ptr<ASTNode> decl() {
  switch (CurTok.type) {
  case (VOID_TOK): // only fun_decl uses "void"
    return fun_decl();
  case (INT_TOK):
  case (FLOAT_TOK):
  case (BOOL_TOK):

    TOKEN lookahead = peekll3();
    switch (lookahead.type) {
    case (SC): // a ";" follows an identifier in a variable declaration
      return var_decl();
    case (LPAR): // a "(" follows an identifier in a function declaration
      return fun_decl();
    }
  }
  TOKEN tok = CurTok;
  LogSyntaxError(tok, "expected a function or variable declaration");
}

// var_decl -> var_type IDENT ";"
std::unique_ptr<VarDeclAST> var_decl() {
  TOKEN type = var_type();
  TOKEN name = CurTok;
  auto variable_declaration = std::make_unique<VarDeclAST>(type, name);
  match(IDENT, "an identifier"); // consume identifier
  match(SC, ";");    // consume ;

  return std::move(variable_declaration);
}

std::unique_ptr<FunctionAST> fun_decl() {
  auto type = type_spec();
  TOKEN identifier = CurTok;

  match(IDENT, "an identifier"); // consume identifier
  match(LPAR, "(");  // consumer identifier
  std::vector<std::unique_ptr<VarDeclAST>> parameters = params();

  match(RPAR, ")");

  // build function definition
  auto scope = block();
  auto prototype =
      std::make_unique<PrototypeAST>(type, identifier, std::move(parameters));
  auto function =
      std::make_unique<FunctionAST>(std::move(prototype), std::move(scope));

  return std::move(function);
}

TOKEN var_type() {
  switch (CurTok.type) {
  case (INT_TOK):
  case (FLOAT_TOK):
  case (BOOL_TOK):
    TOKEN type = CurTok;
    getNextToken(); // consume type token
    return type;
  }
  TOKEN tok = CurTok;
  LogSyntaxError(tok, "expected bool or float or int or void");
}

TOKEN type_spec() {
  if (CurTok.type == VOID_TOK) {
    TOKEN type = CurTok;
    getNextToken(); // consume type token
    return type;
  }
  return var_type();
}

std::vector<std::unique_ptr<VarDeclAST>> params() {

  std::vector<std::unique_ptr<VarDeclAST>> parameter_list;
  switch (CurTok.type) {
  case (RPAR):
    // return empty list to signify no arguments
    return std::move(parameter_list);
  case (BOOL_TOK):
  case (FLOAT_TOK):
  case (INT_TOK):

    param_list(parameter_list);
    return std::move(parameter_list);
  case (VOID_TOK):
    TOKEN tok = CurTok;
    match(VOID_TOK, "void");
    auto void_ = std::make_unique<VarDeclAST>(tok, tok);
    parameter_list.push_back(std::move(void_));
    // parameter_list.push_back(std::move(std::make_unique<VoidASTNode>(tok)));
    return std::move(parameter_list);
  }
  TOKEN tok = CurTok;
  LogSyntaxError(tok, "expected \"(\" or \"bool\" or \"float\" or \"int\" or \"void\"");
}

void param_list(std::vector<std::unique_ptr<VarDeclAST>> &list) {
  auto parameter = param();
  list.push_back(std::move(parameter));
  param_list_prime(list);
}

void param_list_prime(std::vector<std::unique_ptr<VarDeclAST>> &list) {
  if (CurTok.type != RPAR) {
    match(COMMA, ",");
    auto parameter = param();
    list.push_back(std::move(parameter));
    if (CurTok.type == COMMA) {
      param_list_prime(list);
    }
  }
}

std::unique_ptr<VarDeclAST> param() {
  TOKEN type = var_type();
  TOKEN name = CurTok;
  match(IDENT, "an identifier");
  auto parameter = std::make_unique<VarDeclAST>(type, name);
  return std::move(parameter);
}

std::unique_ptr<BlockAST> block() {
  match(LBRA, "{");
  std::vector<std::unique_ptr<ASTNode>> local_declaration_list;
  std::vector<std::unique_ptr<ASTNode>> statement_list;

  // build lists
  local_decls(local_declaration_list);
  stmt_list(statement_list);

  // make copies of the lists
  //  auto local_declarations = local_declaration_list;
  //  auto statements = statement_list;
  TOKEN tok = CurTok;
  match(RBRA, "}");
  auto scope = std::make_unique<BlockAST>(tok, std::move(local_declaration_list),
                                          std::move(statement_list));


  return std::move(scope);
}

void local_decls(std::vector<std::unique_ptr<ASTNode>> &list) {
  local_decls_prime(list);
}

void local_decls_prime(std::vector<std::unique_ptr<ASTNode>> &list) {

  switch (CurTok.type) {
  case (NE):
  case (MOD):
  case (AND):
  case (ASTERIX):
  case (PLUS):
  case (MINUS):
  case (DIV):
  case (SC):
  case (LT):
  case (LE):
  case (GT):
  case (GE):
  case (NOT):
  case (LPAR):
  case (IF):
  case (RETURN):
  case (WHILE):
  case (LBRA):
  case (RBRA):
  case (BOOL_LIT):
  case (FLOAT_LIT):
  case (IDENT):
  case (INT_LIT):
    return;
  }

  auto local_declaration = local_decl();
  list.push_back(std::move(local_declaration));
  local_decls(list);
}

std::unique_ptr<ASTNode> local_decl() {
  TOKEN type = var_type();
  TOKEN identifier = CurTok;
  match(IDENT, "an identifier");
  auto variable_declaration = std::make_unique<VarDeclAST>(type, identifier);
  match(SC, ";");
  return std::move(variable_declaration);
}

void stmt_list(std::vector<std::unique_ptr<ASTNode>> &list) {
  stmt_list_prime(list);
}

void stmt_list_prime(std::vector<std::unique_ptr<ASTNode>> &list) {
  if (CurTok.type != RBRA) {
    auto statement = stmt();
    list.push_back(std::move(statement));
    stmt_list(list);
  }
}

std::unique_ptr<ASTNode> stmt() {
  switch (CurTok.type) {
  case (NOT):
  case (LPAR):
  case (MINUS):
  case (SC):
  case (BOOL_LIT):
  case (FLOAT_LIT):
  case (IDENT):
  case (INT_LIT):
    return std::move(expr_stmt());
  case (LBRA):
    return std::move(block());
  case (IF):
    return std::move(if_stmt());
  case (WHILE):
    return std::move(while_stmt());
  case (RETURN):
    return std::move(return_stmt());
  }
  TOKEN tok = CurTok;
  LogSyntaxError(tok, "expected one of \"!\", \"(\", \"-\", \";\", boolean literal, float literal, integer literal, identifier, \"{\", \"if\", \"while\", \"return\"");
}

std::unique_ptr<ASTNode> expr_stmt() {
  switch (CurTok.type) {
  case (NOT):
  case (LPAR):
  case (MINUS):
  case (SC):
  case (BOOL_LIT):
  case (FLOAT_LIT):
  case (IDENT):
  case (INT_LIT):
    auto expression = expr();
    match(SC, ";");
    return std::move(expression);
  }
  match(SC, ";");
  return nullptr;
}

std::unique_ptr<ASTNode> while_stmt() {
  match(WHILE, "while");
  match(LPAR, "(");
  auto expression = expr();
  match(RPAR, ")");
  auto statement = stmt();

  auto while_statement =
      std::make_unique<WhileAST>(std::move(expression), std::move(statement));

  return std::move(while_statement);
}

std::unique_ptr<ASTNode> if_stmt() {
  match(IF, "if");
  match(LPAR, "(");
  auto expression = expr();
  match(RPAR, ")");
  auto if_block = block();
  auto else_statement = else_stmt();

  auto if_statement = std::make_unique<IfAST>(
      std::move(expression), std::move(if_block), std::move(else_statement));

  return std::move(if_statement);
}

std::unique_ptr<ASTNode> else_stmt() {
  switch (CurTok.type) {
  case (NOT):
  case (LPAR):
  case (MINUS):
  case (SC):
  case (IF):
  case (RETURN):
  case (WHILE):
  case (LBRA):
  case (RBRA):
  case (BOOL_LIT):
  case (FLOAT_LIT):
  case (IDENT):
  case (INT_LIT):
    return nullptr;
  }
  match(ELSE, "else");
  auto else_block = block();
  return std::move(else_block);
}

std::unique_ptr<ASTNode> return_stmt() {
  TOKEN tok = CurTok;
  match(RETURN, "return");
  auto return_body = return_stmt_B();
  auto return_statement = std::make_unique<ReturnAST>(tok,std::move(return_body));

  return std::move(return_statement);
}

std::unique_ptr<ASTNode> return_stmt_B() {
  switch (CurTok.type) {
  case (NOT):
  case (LPAR):
  case (MINUS):
  case (BOOL_LIT):
  case (FLOAT_LIT):
  case (IDENT):
  case (INT_LIT):
    auto expression = expr();
    match(SC, ";");
    return std::move(expression);
  }
  match(SC, ";");
  return nullptr;
}

TOKEN peekll2() {
  TOKEN old = CurTok;
  TOKEN lookahead = getNextToken();
  TOKEN temp = lookahead; // token to return for lookahead
  putBackToken(lookahead);
  CurTok = old;
  return temp;
}

// will need a lookaheads due to IDENT
std::unique_ptr<ASTNode> expr() {
  // determine if it is a variable assignment or a potential variable
  // statement or function call
  if (CurTok.type == IDENT) {

    TOKEN lookahead = peekll2();
    if (lookahead.type == ASSIGN) {
      TOKEN name = CurTok;

      match(IDENT, "an identifier");
      match(ASSIGN, "=");

      auto expression = expr();

      auto variable_assignment =
          std::make_unique<VarAssignAST>(name, std::move(expression));

      return std::move(variable_assignment);
    }
  }
  switch (CurTok.type) {
  case (NOT):
  case (LPAR):
  case (MINUS):
  case (BOOL_LIT):
  case (FLOAT_LIT):
  case (INT_LIT):
  case (IDENT):
    auto expression = or_val();
    return std::move(expression);
  }
  TOKEN tok = CurTok;
  LogSyntaxError(tok, "expected a variable assignment or expression");

}

std::unique_ptr<ASTNode> or_val() {
  auto lhs = and_val();
  while (CurTok.type == OR) {
    TOKEN op = CurTok;
    getNextToken();
    auto rhs = and_val();
    lhs = std::make_unique<BinaryExprAST>(op, std::move(lhs), std::move(rhs));
  }
  return or_val_prime(std::move(lhs));
}

std::unique_ptr<ASTNode> or_val_prime(std::unique_ptr<ASTNode> lhs) {
  TOKEN op;

  switch (CurTok.type) {
  case (RPAR):
  case (SC):
  case (COMMA):
    return std::move(lhs);
  case (OR):
    op = CurTok;
    getNextToken();

    auto rhs = and_val();
    rhs = or_val_prime(std::move(rhs));
    auto bin_op =
        std::make_unique<BinaryExprAST>(op, std::move(lhs), std::move(rhs));
    return std::move(bin_op);
  }
  TOKEN tok = CurTok;  
  LogSyntaxError(tok, "expected one of \")\", \";\", \"||\"");
}

std::unique_ptr<ASTNode> and_val() {

  auto lhs = eq_val();
  while (CurTok.type == AND) {
    TOKEN op = CurTok;
    getNextToken();
    auto rhs = eq_val();
    lhs = std::make_unique<BinaryExprAST>(op, std::move(lhs), std::move(rhs));
  }
  return and_val_prime(std::move(lhs));
}

std::unique_ptr<ASTNode> and_val_prime(std::unique_ptr<ASTNode> lhs) {
  TOKEN op;
  switch (CurTok.type) {
  case (RPAR):
  case (SC):
  case (OR):
  case (COMMA):
    return std::move(lhs);
  case (AND):
    op = CurTok;
    getNextToken();

    auto rhs = eq_val();
    rhs = and_val_prime(std::move(rhs));

    auto bin_op =
        std::make_unique<BinaryExprAST>(op, std::move(lhs), std::move(rhs));
    return std::move(bin_op);
  }
  TOKEN tok = CurTok;
  LogSyntaxError(tok, "expected one of \")\", \";\", \"||\", \"&&\"");
}

std::unique_ptr<ASTNode> eq_val() {
  auto lhs = comp_val();

  while (CurTok.type == EQ || CurTok.type == NE) {
    TOKEN op = CurTok;
    getNextToken();
    auto rhs = comp_val();
    lhs = std::make_unique<BinaryExprAST>(op, std::move(lhs), std::move(rhs));
  }

  return eq_val_prime(std::move(lhs));
}

std::unique_ptr<ASTNode> eq_val_prime(std::unique_ptr<ASTNode> lhs) {
  TOKEN op;
  switch (CurTok.type) {
  case (AND):
  case (RPAR):
  case (SC):
  case (OR):
  case (COMMA):
    return std::move(lhs);
  case (EQ):
  case (NE):
    op = CurTok;
    getNextToken();

    auto rhs = comp_val();
    rhs = eq_val_prime(std::move(rhs));

    auto bin_op =
        std::make_unique<BinaryExprAST>(op, std::move(lhs), std::move(rhs));
    return std::move(bin_op);
  }
  TOKEN tok = CurTok;
  LogSyntaxError(tok, "expected one of \")\", \";\", \"||\", \"&&\", \"==\", \"!=\"");
}

std::unique_ptr<ASTNode> comp_val() {
  auto lhs = add_val();
  while (CurTok.type == LE || CurTok.type == LT || CurTok.type == GE ||
         CurTok.type == GT) {
    TOKEN op = CurTok;
    getNextToken();
    auto rhs = add_val();
    lhs = std::make_unique<BinaryExprAST>(op, std::move(lhs), std::move(rhs));
  }
  return comp_val_prime(std::move(lhs));
}

std::unique_ptr<ASTNode> comp_val_prime(std::unique_ptr<ASTNode> lhs) {
  TOKEN op;
  switch (CurTok.type) {
  case (NE):
  case (AND):
  case (RPAR):
  case (SC):
  case (EQ):
  case (OR):
  case (COMMA):
    return std::move(lhs);
  case (LE):
  case (LT):
  case (GE):
  case (GT):
    op = CurTok;
    getNextToken();

    auto rhs = add_val();
    rhs = comp_val_prime(std::move(rhs));

    auto bin_op =
        std::make_unique<BinaryExprAST>(op, std::move(lhs), std::move(rhs));
    return std::move(bin_op);
  }
  TOKEN tok = CurTok;
  LogSyntaxError(tok, "expected one of \")\", \";\", \"||\", \"&&\", \"<\", \"<=\", \">\", \">=\"");
}

std::unique_ptr<ASTNode> add_val() {
  auto lhs = mul_val();

  while (CurTok.type == PLUS || CurTok.type == MINUS) {
    TOKEN op = CurTok;
    getNextToken();
    auto rhs = mul_val();
    lhs = std::make_unique<BinaryExprAST>(op, std::move(lhs), std::move(rhs));
  }
  return add_val_prime(std::move(lhs));
}

std::unique_ptr<ASTNode> add_val_prime(std::unique_ptr<ASTNode> lhs) {
  TOKEN op;
  switch (CurTok.type) {
  case (NE):
  case (AND):
  case (RPAR):
  case (SC):
  case (LT):
  case (LE):
  case (EQ):
  case (GT):
  case (GE):
  case (OR):
  case (COMMA):
    return std::move(lhs);
  case (PLUS):
  case (MINUS):
    op = CurTok;
    getNextToken();
    auto rhs = mul_val();
    rhs = add_val_prime(std::move(rhs));
    auto bin_op =
        std::make_unique<BinaryExprAST>(op, std::move(lhs), std::move(rhs));
    return std::move(bin_op);
  }
  TOKEN tok = CurTok;
  LogSyntaxError(tok, "expected one of \")\", \";\", \"||\", \"&&\", \"<\", \"<=\", \">\", \">=\", \"+\", \"-\"");

}

std::unique_ptr<ASTNode> mul_val() {
  auto lhs = unary();
  while (CurTok.type == ASTERIX || CurTok.type == DIV || CurTok.type == MOD) {
    TOKEN op = CurTok;
    getNextToken();
    auto rhs = unary();
    lhs = std::make_unique<BinaryExprAST>(op, std::move(lhs), std::move(rhs));
  }
  return mul_val_prime(std::move(lhs));
}

std::unique_ptr<ASTNode> mul_val_prime(std::unique_ptr<ASTNode> lhs) {
  TOKEN op;

  switch (CurTok.type) {
  case (NE):
  case (AND):
  case (RPAR):
  case (PLUS):
  case (MINUS):
  case (SC):
  case (LT):
  case (LE):
  case (EQ):
  case (GT):
  case (GE):
  case (OR):
  case (COMMA):
    return std::move(lhs);
  case (ASTERIX):
  case (DIV):
  case (MOD):
    op = CurTok;
    getNextToken();
    auto rhs = unary();
    rhs = mul_val_prime(std::move(rhs));
    auto bin_op =
        std::make_unique<BinaryExprAST>(op, std::move(lhs), std::move(rhs));
    return std::move(bin_op);
  }
  TOKEN tok = CurTok;
  LogSyntaxError(tok, "expected one of \")\", \";\", \"||\", \"&&\", \"<\", \"<=\", \">\", \">=\", \"+\", \"-\", \"*\", \"/\", \"%\"");
}

std::unique_ptr<ASTNode> unary() {
  switch (CurTok.type) {
  case (LPAR):
  case (BOOL_LIT):
  case (FLOAT_LIT):
  case (IDENT):
  case (INT_LIT): {
    auto identifier = identifiers();
    return std::move(identifier);
  }
  case (NOT):
  case (MINUS):
    TOKEN op = CurTok;
    getNextToken();
    auto unary_expression =
        std::make_unique<UnaryExprAST>(op, std::move(unary()));

    return std::move(unary_expression);
  }
  TOKEN tok = CurTok;
  LogSyntaxError(tok, "expected one of \"(\", boolean literal, float literal, indentifier, int literal, \"-\", \"!\"");
}

std::unique_ptr<ASTNode> identifiers() {
  switch (CurTok.type) {
  case (IDENT): {
    auto identifier = identifiers_B();
    return std::move(identifier);
  }
  case (INT_LIT):
  case (FLOAT_LIT):
  case (BOOL_LIT): {
    TOKEN tok = CurTok;
    getNextToken();
    auto result = std::make_unique<LiteralASTNode>(tok);
    return std::move(result);
  }
  case (LPAR):
    match(LPAR, "(");
    auto expression = expr();
    match(RPAR, ")");
    return std::move(expression);
  }
  TOKEN tok = CurTok;
  LogSyntaxError(tok, "expected one of \"(\", boolean literal, float literal, indentifier, int literal");

}

std::unique_ptr<ASTNode> identifiers_B() {
  TOKEN identifier = CurTok;
  getNextToken();
  switch (CurTok.type) {
  case (NE):
  case (AND):
  case (RPAR):
  case (PLUS):
  case (MINUS):
  case (SC):
  case (LT):
  case (LE):
  case (EQ):
  case (GT):
  case (GE):
  case (OR):
  case (COMMA):
  case (ASTERIX):
  case (DIV):
  case (MOD): {
    auto result = std::make_unique<LiteralASTNode>(identifier);
    return std::move(result);
  }
  case (LPAR):
    // function call
    TOKEN callee = identifier;

    match(LPAR, "(");
    std::vector<std::unique_ptr<ASTNode>> arguments = args();

    match(RPAR, ")");
    auto function_call =
        std::make_unique<CallExprAST>(callee, std::move(arguments));
    return std::move(function_call);
  }
  TOKEN tok = CurTok;
  LogSyntaxError(tok, "expected one of \"(\", \")\", \";\", \"||\", \"&&\", \"<\", \"<=\", \">\", \">=\", \"+\", \"-\", \"*\", \"/\", \"%\"");

}

std::vector<std::unique_ptr<ASTNode>> args() {
  std::vector<std::unique_ptr<ASTNode>> argument_list;
  // if not the follow case, we will populate the list otherwise we simply
  // return an empty list to show we have no args
  if (CurTok.type != RPAR) {
    arg_list(argument_list);
  }

  return std::move(argument_list);
}

void arg_list(std::vector<std::unique_ptr<ASTNode>> &list) {
  auto expression = expr();
  list.push_back(std::move(expression));

  arg_list_prime(list);
}

void arg_list_prime(std::vector<std::unique_ptr<ASTNode>> &list) {
  if (CurTok.type != RPAR) {
    match(COMMA, ",");
    auto expression = expr();
    list.push_back(std::move(expression));
    arg_list_prime(list);
  }
}