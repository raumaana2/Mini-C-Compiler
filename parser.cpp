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

/**
 * get next token from buffer
 */
TOKEN getNextToken() {

  if (tok_buffer.size() == 0)
    tok_buffer.push_back(gettok());

  TOKEN temp = tok_buffer.front();
  tok_buffer.pop_front();

  return CurTok = temp;
}

/**
 * @brief  
 *    put back token into buffer
 * @param  tok: token to put back into buffer
 * @retval None
 */
void putBackToken(TOKEN tok) { tok_buffer.push_front(tok); }


/**
 * @brief  
 *    checks if curtok matches token we want to consume and if they dont match
 *          then throw a syntax error
 * @param  word: symbol to match
 * @param  symbol: symbol that needed to be matched, used by LogSymbolError(tok, symbol)
 * @retval None
 */
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

/**
 * @brief  
 *    begin parser here 
 * @retval a unique pointer to program ast
 */
std::unique_ptr<ASTNode> parser() {
  auto program_scope = program();

  switch (CurTok.type) {
  case (EOF_TOK):
    return std::move(program_scope);
  default:
    TOKEN tok = CurTok;
    LogSyntaxError(tok, "Expected EOF");
    return nullptr;
  }
}

// program -> extern_list decl_list | decl_list
/**
 * @brief  
 *   builds list for either extern list and decl list or just decl list
 * @retval unique pointer to ast node
 */
std::unique_ptr<ASTNode> program() {
  std::vector<std::unique_ptr<ASTNode>> extern_vector;
  std::vector<std::unique_ptr<ASTNode>> decl_vector;
  
  switch (CurTok.type) {
  case (EXTERN):  // first set that choose extern_list and decl_list
    extern_list(extern_vector);
  case (BOOL_TOK):
  case (FLOAT_TOK):
  case (INT_TOK):
  case (VOID_TOK): { //first set that chooses only decl_list
    decl_list(decl_vector);
    auto scope = std::make_unique<ProgramAST>(std::move(extern_vector),
                                            std::move(decl_vector));
    return std::move(scope);
  }
  
  }
  TOKEN tok = CurTok;
  LogSyntaxError(tok, "expected extern or bool or float or int or void");
  return nullptr;
}



// extern_list -> extern extern_list_prime
/**
 * @brief  
 *    builds extern list
 * @param  &list: extern list 
 * @retval None
 */
void extern_list(std::vector<std::unique_ptr<ASTNode>> &list) {
  auto _extern_ = extern_();  
  list.push_back(std::move(_extern_));
  extern_list_prime(list);
}

// extern_list_prime -> extern extern_list_prime | epsilon
/**
 * @brief  
 *    builds extern list 
 * @param  &list: extern_list 
 * @retval None
 */
void extern_list_prime(std::vector<std::unique_ptr<ASTNode>> &list) {

  switch (CurTok.type) {
  case (INT_TOK):
  case (FLOAT_TOK):
  case (BOOL_TOK):
  case (VOID_TOK):
    return; //follow set meaning we should terminate list building 
  case (EXTERN):  // first set so we continue list building
    auto _extern_ = extern_();
    list.push_back(std::move(_extern_));
    extern_list_prime(list);
    return;
  }
  TOKEN tok = CurTok;
  LogSyntaxError(tok, "expected extern or bool or float or int or void");
}

// extern -> "extern" type_spec IDENT "(" params ")" ";"
/**
 * @brief  
 *    build extern function definition
 * @retval unique pointer to prototype ast
 */
std::unique_ptr<ASTNode> extern_() {
  match(EXTERN, "extern");
  auto type = type_spec();

  TOKEN identifier = CurTok;
  match(IDENT, "an identifier"); // consume identifier

  match(LPAR, "("); // consume (

  auto parameters = params();
  match(RPAR, ")");
  match(SC, ";");
  auto prototype =
      std::make_unique<PrototypeAST>(type, identifier, std::move(parameters));

  return std::move(prototype);
}

// decl_list -> decl decl_list_prime
/**
 * @brief  
 *    build decl list
 * @param  &list: decl_list
 * @retval None
 */
void decl_list(std::vector<std::unique_ptr<ASTNode>> &list) {

  auto declaration = decl();
  list.push_back(std::move(declaration));
  decl_list_prime(list);
}

// decl_list -> decl decl_list_prime | epsilon
/**
 * @brief  
 *    build decl list
 * @param  &list: decl_list
 * @retval None
 */
void decl_list_prime(std::vector<std::unique_ptr<ASTNode>> &list) {
  if (CurTok.type != EOF_TOK) { //if eof token, this is part of follow set of decl_list_prime so we stop list building
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
/**
 * @brief  
 *    decl function that decides between function declaration or variable declaration
 * @retval unique pointer to variable or function declaration
 */

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
  return nullptr;
}

// var_decl -> var_type IDENT ";"
/**
 * @brief  
 *    function to deal with variable declarations
 * @retval unique pointer to variable declaration 
 */
std::unique_ptr<VarDeclAST> var_decl() {
  TOKEN type = var_type();
  TOKEN name = CurTok;
  auto variable_declaration = std::make_unique<VarDeclAST>(type, name);
  match(IDENT, "an identifier"); // consume identifier
  match(SC, ";");    // consume ;

  return std::move(variable_declaration);
}

//fun_decl -> type_spec IDENT "(" params ")" block
/**
 * @brief  
 *    function to deal with function declaration 
 * @retval unique pointer to function declaration 
 */
std::unique_ptr<FunctionAST> fun_decl() {
  auto type = type_spec();
  TOKEN identifier = CurTok;

  match(IDENT, "an identifier"); 
  match(LPAR, "(");  
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

//var_type -> "int" | "float" | "bool"
/**
 * @brief  
 *   type that can be used by variable or function declaration 
 * @retval token of type
 */
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
  TOKEN null_tok;
  return null_tok;
}

//type_spec -> "void"| var_type
/**
 * @brief  
 *    function declaration is only allowed void type otherwise check if int, float or bool
 * @retval token of type
 */

TOKEN type_spec() {
  if (CurTok.type == VOID_TOK) {
    TOKEN type = CurTok;
    getNextToken(); // consume type token
    return type;
  }
  return var_type();
}

//params -> param_list | "void" | ϵ
/**
 * @brief  
 *   function used to build parameter list or check if no parameters or if has void parameter
 * @retval unique pointer 
 */
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
  case (VOID_TOK):  //void tok is special case so we just push a token of void type
    TOKEN tok = CurTok;
    match(VOID_TOK, "void");
    auto void_ = std::make_unique<VarDeclAST>(tok, tok);
    parameter_list.push_back(std::move(void_));
    return std::move(parameter_list);
  }
  TOKEN tok = CurTok;
  LogSyntaxError(tok, "expected \"(\" or \"bool\" or \"float\" or \"int\" or \"void\"");
}



// param_list -> param param_list_prime
/**
 * @brief  
 *    builds list of parameters
 * @param  &list: parameter list 
 * @retval None
 */
void param_list(std::vector<std::unique_ptr<VarDeclAST>> &list) {
  auto parameter = param();
  list.push_back(std::move(parameter));
  param_list_prime(list);
}

//param_list_prime -> "," param param_list_prime | ϵ
/**
 * @brief  
 *    builds list of parameters
 * @param  &list: parameter list 
 * @retval None
 */
void param_list_prime(std::vector<std::unique_ptr<VarDeclAST>> &list) {
  if (CurTok.type != RPAR) {  //when ")", then there are no more arguments
    match(COMMA, ",");
    auto parameter = param();
    list.push_back(std::move(parameter));
    if (CurTok.type == COMMA) {
      param_list_prime(list);
    }
  }
}

//param -> var_type IDENT
/**
 * @brief  
 *    builds parameter
 * @retval unique pointer to parameter represented as variable declaration
 */
std::unique_ptr<VarDeclAST> param() {
  TOKEN type = var_type();
  TOKEN name = CurTok;
  match(IDENT, "an identifier");
  auto parameter = std::make_unique<VarDeclAST>(type, name);
  return std::move(parameter);
}


//block -> "{" local_decls stmt_list "}"
/**
 * @brief  
 *    builds block ast with local declaration list and statement list
 * @retval unique pointer to block ast
 */
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


//local_decls -> local_decls_prime
/**
 * @brief  
 *    builds list of local variable declarations
 * @param  &list: list of local variable declarations
 * @retval None
 */
void local_decls(std::vector<std::unique_ptr<ASTNode>> &list) {
  local_decls_prime(list);
}

// local_decls_prime -> local_decl local_decls_prime | ϵ
/**
 * @brief  
 *    builds list of local variable declarations
 * @param  &list: list of local variable declarations
 * @retval None
 */
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
    return;//follow set so stop list building
  }

  auto local_declaration = local_decl();
  list.push_back(std::move(local_declaration));
  local_decls(list);
}

//local_decl -> var_type IDENT ";"
/**
 * @brief  
 *  build local variable declaration    
 * @retval unique pointer to variable declaration ast
 */
std::unique_ptr<ASTNode> local_decl() {
  TOKEN type = var_type();
  TOKEN identifier = CurTok;
  match(IDENT, "an identifier");
  auto variable_declaration = std::make_unique<VarDeclAST>(type, identifier);
  match(SC, ";");
  return std::move(variable_declaration);
}

//stmt_list -> stmt_list_prime
/**
 * @brief  
 *    build list of statements
 * @param  &list: list of statements
 * @retval None
 */
void stmt_list(std::vector<std::unique_ptr<ASTNode>> &list) {
  stmt_list_prime(list);
}

// stmt_list_prime -> stmt stmt_list_prime | ϵ
//stmt_list -> stmt_list_prime
/**
 * @brief  
 *    build list of statements
 * @param  &list: list of statements
 * @retval None
 */
void stmt_list_prime(std::vector<std::unique_ptr<ASTNode>> &list) {
  if (CurTok.type != RBRA) {  //if "}" then we have reached end of block
    auto statement = stmt();
    list.push_back(std::move(statement));
    stmt_list(list);
  }
}


//stmt -> expr_stmt | block | if_stmt | while_stmt | return_stmt
/**
 * @brief  
 *  build statement   
 * @retval unique pointer to if statement, expression statement, block, while statement or return statement ast
 */
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
  return nullptr;
}


// expr_stmt -> expr "; | ";"
/**
 * @brief  
 *    build expression 
 * @retval unique pointer to some expression ast
 */
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


//while_stmt -> "while" "(" expr ")" stmt
/**
 * @brief  
 *    build while statement
 * @retval unique pointer to while statement ast
 */
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


//if_stmt -> "if" "(" expr ")" block else_stmt
/**
 * @brief  
 *  build if statement   
 * @retval unique pointer to if statement ast
 */
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


// else_stmt -> "else" block| ϵ
/**
 * @brief  
 *    build else statement or return null ptr if doesn't exist
 * @retval potentially a unique pointer to else body else a nullptr
 */
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
  case (INT_LIT): //follow set suggests that there is no else body
    return nullptr;
  }
  match(ELSE, "else");
  auto else_block = block();
  return std::move(else_block);
}

//return_stmt -> "return" return_stmt_B
/**
 * @brief  
 *  build return statement 
 * @retval unique pointer to return statement ast
 */
std::unique_ptr<ASTNode> return_stmt() {
  TOKEN tok = CurTok;
  match(RETURN, "return");
  auto return_body = return_stmt_B();
  auto return_statement = std::make_unique<ReturnAST>(tok,std::move(return_body));

  return std::move(return_statement);
}

//return_stmt_B -> "; | expr ";"
/**
 * @brief  
 *  build return body or check if no such body   
 * @retval nullptr if no body otherwise unique pointer to return statement body 
 */
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
  //no body found
  match(SC, ";"); 
  return nullptr;
}

//ll2 peek to determine between assignment or expression 
TOKEN peekll2() {
  TOKEN old = CurTok;
  TOKEN lookahead = getNextToken();
  TOKEN temp = lookahead; // token to return for lookahead
  putBackToken(lookahead);
  CurTok = old;
  return temp;
}

//expr -> IDENT "=" expr | or_val
/**
 * @brief  
 *  build expression that is either expression or assignment   
 * @retval unique pointer to expression ast or assignement ast
 */
std::unique_ptr<ASTNode> expr() {
  // determine if it is a variable assignment or a potential variable
  // statement or function call
  if (CurTok.type == IDENT) {
    // will need a lookaheads due to IDENT
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
  //determined it maybe an expression 
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
  return nullptr;
}

//or_val -> and_val or_val_prime 
/**
 * @brief  
 *  build expression   
 * @retval unique pointer to binary expression 
 */
std::unique_ptr<ASTNode> or_val() {
  auto lhs = and_val();

  //code to deal with right associativity 
  while (CurTok.type == OR) {
    TOKEN op = CurTok;
    getNextToken();
    auto rhs = and_val();
    lhs = std::make_unique<BinaryExprAST>(op, std::move(lhs), std::move(rhs));
  }
  return or_val_prime(std::move(lhs));
}


// or_val_prime -> "||" and_val or_val_prime | ϵ
/**
 * @brief  
 *  build expression   
 * @retval unique pointer to binary expression 
 */
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
  return nullptr;
}

// and_val -> eq_val and_val_prime
/**
 * @brief  
 *  build expression   
 * @retval unique pointer to binary expression 
 */
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


// and_val_prime -> "&&" eq_val and_val_prime | ϵ
/**
 * @brief  
 *  build expression   
 * @retval unique pointer to binary expression 
 */
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
  return nullptr;
}


//eq_val -> comp_val eq_val_prime
/**
 * @brief  
 *  build expression   
 * @retval unique pointer to binary expression 
 */
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

// eq_val_prime -> "==" comp_val eq_val_prime | "!=" comp_val eq_val_prime | ϵ
/**
 * @brief  
 *  build expression   
 * @retval unique pointer to binary expression 
 */
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
  return nullptr;
}


// comp_val -> add_val comp_val_prime
/**
 * @brief  
 *  build expression   
 * @retval unique pointer to binary expression 
 */
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

// comp_val_prime -> "<=" add_val comp_val_prime
// | "<" add_val comp_val_prime
// | ">=" add_val comp_val_prime
// | ">" add_val comp_val_prime
// | ϵ
/**
 * @brief  
 *  build expression   
 * @retval unique pointer to binary expression 
 */
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
  return nullptr;
}

// add_val -> mul_val add_val_prime
/**
 * @brief  
 *  build expression   
 * @retval unique pointer to binary expression 
 */
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


// add_val_prime -> "+" mul_val add_val_prime
// | "-" mul_val add_val_prime
// | ϵ
/**
 * @brief  
 *  build expression   
 * @retval unique pointer to binary expression 
 */
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
  return nullptr;

}


// mul_val -> unary mul_val_prime
/**
 * @brief  
 *  build expression   
 * @retval unique pointer to binary expression 
 */
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


// mul_val_prime ->  "*" unary mul_val_prime
// | "/" unary mul_val_prime
// | "%" unary mul_val_prime
// | ϵ
/**
 * @brief  
 *  build expression   
 * @retval unique pointer to binary expression 
 */
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
  return nullptr;
}


// unary ->  "-" unary
// | "!" unary
// | identifiers
/**
 * @brief  
 *  build unaary expression   
 * @retval unique pointer to unary expression 
 */
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
  return nullptr;
}


// identifiers -> "(" expr ")"
// | IDENT identifiers_B
// | INT_LIT 
// | FLOAT_LIT
// | BOOL_LIT
/**
 * @brief  
 *  determines if identifier or literal 
 * @retval unique pointer to literal ast
 */
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
  return nullptr;

}



// identifiers_B -> "(" args ")"
// | ϵ 
/**
 * @brief  
 *  determines if identifier or function call
 * @retval unique pointer to identifier or function call
 */
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
  return nullptr;
}

// args -> arg_list
// | ϵ
/**
 * @brief  
 * build arguments for argument list
 * @retval unique pointer to argument
 */
std::vector<std::unique_ptr<ASTNode>> args() {
  std::vector<std::unique_ptr<ASTNode>> argument_list;
  // if not the follow case, we will populate the list otherwise we simply
  // return an empty list to show we have no args
  if (CurTok.type != RPAR) {
    arg_list(argument_list);
  }

  return std::move(argument_list);
}


// arg_list -> expr arg_list_prime
/**
 * @brief  
 *   build list of arguments 
 * @param  &list: argument list 
 * @retval None
 */
void arg_list(std::vector<std::unique_ptr<ASTNode>> &list) {
  auto expression = expr();
  list.push_back(std::move(expression));

  arg_list_prime(list);
}

// arg_list_prime -> "," expr arg_list_prime | ϵ
/**
 * @brief  
 *  build list of arguments
 * @param  &list: 
 * @retval None
 */
void arg_list_prime(std::vector<std::unique_ptr<ASTNode>> &list) {
  if (CurTok.type != RPAR) { //if ")" then no more arguments left, stop building
    match(COMMA, ",");
    auto expression = expr();
    list.push_back(std::move(expression));
    arg_list_prime(list);
  }
}