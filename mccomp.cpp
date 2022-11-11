#include "mccomp.hpp"
#include <typeinfo>
using namespace llvm;
using namespace llvm::sys;



FILE *pFile;

//===----------------------------------------------------------------------===//
// Lexer
//===----------------------------------------------------------------------===//


static std::string IdentifierStr; // Filled in if IDENT
static int IntVal;                // Filled in if INT_LIT
static bool BoolVal;              // Filled in if BOOL_LIT
static float FloatVal;            // Filled in if FLOAT_LIT
static std::string StringVal;     // Filled in if String Literal
static int lineNo, columnNo;

static TOKEN returnTok(std::string lexVal, int tok_type) {
  TOKEN return_tok;
  return_tok.lexeme = lexVal;
  return_tok.type = tok_type;
  return_tok.lineNo = lineNo;
  return_tok.columnNo = columnNo - lexVal.length() - 1;
  return return_tok;
}

// Read file line by line -- or look for \n and if found add 1 to line number
// and reset column number to 0
/// gettok - Return the next token from standard input.
static TOKEN gettok() {

  static int LastChar = ' ';
  static int NextChar = ' ';

  // Skip any whitespace.
  while (isspace(LastChar)) {
    if (LastChar == '\n' || LastChar == '\r') {
      lineNo++;
      columnNo = 1;
    }
    LastChar = getc(pFile);
    columnNo++;
  }

  if (isalpha(LastChar) ||
      (LastChar == '_')) { // identifier: [a-zA-Z_][a-zA-Z_0-9]*
    IdentifierStr = LastChar;
    columnNo++;

    while (isalnum((LastChar = getc(pFile))) || (LastChar == '_')) {
      IdentifierStr += LastChar;
      columnNo++;
    }

    if (IdentifierStr == "int")
      return returnTok("int", INT_TOK);
    if (IdentifierStr == "bool")
      return returnTok("bool", BOOL_TOK);
    if (IdentifierStr == "float")
      return returnTok("float", FLOAT_TOK);
    if (IdentifierStr == "void")
      return returnTok("void", VOID_TOK);
    if (IdentifierStr == "bool")
      return returnTok("bool", BOOL_TOK);
    if (IdentifierStr == "extern")
      return returnTok("extern", EXTERN);
    if (IdentifierStr == "if")
      return returnTok("if", IF);
    if (IdentifierStr == "else")
      return returnTok("else", ELSE);
    if (IdentifierStr == "while")
      return returnTok("while", WHILE);
    if (IdentifierStr == "return")
      return returnTok("return", RETURN);
    if (IdentifierStr == "true") {
      BoolVal = true;
      return returnTok("true", BOOL_LIT);
    }
    if (IdentifierStr == "false") {
      BoolVal = false;
      return returnTok("false", BOOL_LIT);
    }

    return returnTok(IdentifierStr.c_str(), IDENT);
  }

  if (LastChar == '=') {
    NextChar = getc(pFile);
    if (NextChar == '=') { // EQ: ==
      LastChar = getc(pFile);
      columnNo += 2;
      return returnTok("==", EQ);
    } else {
      LastChar = NextChar;
      columnNo++;
      return returnTok("=", ASSIGN);
    }
  }

  if (LastChar == '{') {
    LastChar = getc(pFile);
    columnNo++;
    return returnTok("{", LBRA);
  }
  if (LastChar == '}') {
    LastChar = getc(pFile);
    columnNo++;
    return returnTok("}", RBRA);
  }
  if (LastChar == '(') {
    LastChar = getc(pFile);
    columnNo++;
    return returnTok("(", LPAR);
  }
  if (LastChar == ')') {
    LastChar = getc(pFile);
    columnNo++;
    return returnTok(")", RPAR);
  }
  if (LastChar == ';') {
    LastChar = getc(pFile);
    columnNo++;
    return returnTok(";", SC);
  }
  if (LastChar == ',') {
    LastChar = getc(pFile);
    columnNo++;
    return returnTok(",", COMMA);
  }

  if (isdigit(LastChar) || LastChar == '.') { // Number: [0-9]+.
    std::string NumStr;

    if (LastChar == '.') { // Floatingpoint Number: .[0-9]+
      do {
        NumStr += LastChar;
        LastChar = getc(pFile);
        columnNo++;
      } while (isdigit(LastChar));

      FloatVal = strtof(NumStr.c_str(), nullptr);
      return returnTok(NumStr, FLOAT_LIT);
    } else {
      do { // Start of Number: [0-9]+
        NumStr += LastChar;
        LastChar = getc(pFile);
        columnNo++;
      } while (isdigit(LastChar));

      if (LastChar == '.') { // Floatingpoint Number: [0-9]+.[0-9]+)
        do {
          NumStr += LastChar;
          LastChar = getc(pFile);
          columnNo++;
        } while (isdigit(LastChar));

        FloatVal = strtof(NumStr.c_str(), nullptr);
        return returnTok(NumStr, FLOAT_LIT);
      } else { // Integer : [0-9]+
        IntVal = strtod(NumStr.c_str(), nullptr);
        return returnTok(NumStr, INT_LIT);
      }
    }
  }

  if (LastChar == '&') {
    NextChar = getc(pFile);
    if (NextChar == '&') { // AND: &&
      LastChar = getc(pFile);
      columnNo += 2;
      return returnTok("&&", AND);
    } else {
      LastChar = NextChar;
      columnNo++;
      return returnTok("&", int('&'));
    }
  }

  if (LastChar == '|') {
    NextChar = getc(pFile);
    if (NextChar == '|') { // OR: ||
      LastChar = getc(pFile);
      columnNo += 2;
      return returnTok("||", OR);
    } else {
      LastChar = NextChar;
      columnNo++;
      return returnTok("|", int('|'));
    }
  }

  if (LastChar == '!') {
    NextChar = getc(pFile);
    if (NextChar == '=') { // NE: !=
      LastChar = getc(pFile);
      columnNo += 2;
      return returnTok("!=", NE);
    } else {
      LastChar = NextChar;
      columnNo++;
      return returnTok("!", NOT);
      ;
    }
  }

  if (LastChar == '<') {
    NextChar = getc(pFile);
    if (NextChar == '=') { // LE: <=
      LastChar = getc(pFile);
      columnNo += 2;
      return returnTok("<=", LE);
    } else {
      LastChar = NextChar;
      columnNo++;
      return returnTok("<", LT);
    }
  }

  if (LastChar == '>') {
    NextChar = getc(pFile);
    if (NextChar == '=') { // GE: >=
      LastChar = getc(pFile);
      columnNo += 2;
      return returnTok(">=", GE);
    } else {
      LastChar = NextChar;
      columnNo++;
      return returnTok(">", GT);
    }
  }

  if (LastChar == '/') { // could be division or could be the start of a comment
    LastChar = getc(pFile);
    columnNo++;
    if (LastChar == '/') { // definitely a comment
      do {
        LastChar = getc(pFile);
        columnNo++;
      } while (LastChar != EOF && LastChar != '\n' && LastChar != '\r');

      if (LastChar != EOF)
        return gettok();
    } else
      return returnTok("/", DIV);
  }

  // Check for end of file.  Don't eat the EOF.
  if (LastChar == EOF) {
    columnNo++;
    return returnTok("0", EOF_TOK);
  }

  // Otherwise, just return the character as its ascii value.
  int ThisChar = LastChar;
  std::string s(1, ThisChar);
  LastChar = getc(pFile);
  columnNo++;
  return returnTok(s, int(ThisChar));
}

//===----------------------------------------------------------------------===//
// Parser
//===----------------------------------------------------------------------===//

/// CurTok/getNextToken - Provide a simple token buffer.  CurTok is the current
/// token the parser is looking at.  getNextToken reads another token from the
/// lexer and updates CurTok with its results.
static TOKEN CurTok;
static std::deque<TOKEN> tok_buffer;

static TOKEN getNextToken() {

  if (tok_buffer.size() == 0)
    tok_buffer.push_back(gettok());

  TOKEN temp = tok_buffer.front();
  tok_buffer.pop_front();

  return CurTok = temp;
}

static void putBackToken(TOKEN tok) { tok_buffer.push_front(tok); }

void match(int word) {
  if (CurTok.type == word) {
    // std::cerr << "matched " << CurTok.lexeme << std::endl;
    getNextToken();
  } else {
    std::cerr << "Expected token " << word << " but got " << CurTok.type << std::endl;
    exit(0);
  }

}

//===----------------------------------------------------------------------===//
// Recursive Descent Parser - Function call for each production
//===----------------------------------------------------------------------===//


std::unique_ptr<ast_node> parser() {
  auto program_scope = program();

  switch (CurTok.type) {
    case (EOF_TOK):
      return std::move(program_scope);
    default:
      //error
      exit(0);
  }
  
}


// program -> extern_list decl_list | decl_list
std::unique_ptr<ast_node> program() {
  std::vector<std::unique_ptr<ast_node>> extern_vector;
  std::vector<std::unique_ptr<ast_node>> decl_vector;

  switch (CurTok.type) {
    case (EXTERN):
      extern_list(extern_vector);
    case (BOOL_TOK):
    case (FLOAT_TOK):
    case (INT_TOK):
    case (VOID_TOK):
    {
      decl_list(decl_vector);
      auto scope = std::make_unique<scope_ast>(std::move(extern_vector), std::move(decl_vector));
      return std::move(scope);

    }
    default:
      std::cerr << "Expected extern or bool or float or int or void " << std::endl;

      exit(0);

  }

}

// extern_list -> extern extern_list_prime
void extern_list(std::vector<std::unique_ptr<ast_node>>& list) {
  auto _extern_ = extern_();
  list.push_back(std::move(_extern_));
  extern_list_prime(list);
}


//extern_list_prime -> extern extern_list_prime | epsilon 
void extern_list_prime(std::vector<std::unique_ptr<ast_node>>& list) {

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

  std::cerr << "Expected extern or int or float or bool or void" << std::endl;
  exit(0);
}

// extern -> "extern" type_spec IDENT "(" params ")" ";"
std::unique_ptr<ast_node> extern_() {
  match(EXTERN);
  auto type = type_spec();

  TOKEN identifier = CurTok;
  match(IDENT); //consume identifier

  match(LPAR);  //consumer (

 

  auto parameters = params();
  match(RPAR);
  match(SC);
  auto prototype = std::make_unique<prototype_ast>(type, identifier, std::move(parameters));

  return std::move(prototype);
}


// decl_list -> decl decl_list_prime
void decl_list(std::vector<std::unique_ptr<ast_node>>& list) {

  auto declaration = decl();
  list.push_back(std::move(declaration));
  decl_list_prime(list);
}


// decl_list -> decl decl_list_prime | epsilon
void decl_list_prime(std::vector<std::unique_ptr<ast_node>>& list) {
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


  TOKEN temp = lookahead2; //token to return for lookahead

  putBackToken(lookahead2);
  putBackToken(lookahead1);

  CurTok = old;
  
  return temp;
}

// decl -> var_decl | fun_decl
std::unique_ptr<ast_node> decl() { 
  switch (CurTok.type) {
    case (VOID_TOK):  //only fun_decl uses "void"
      return fun_decl();
    case (INT_TOK):
    case (FLOAT_TOK):
    case (BOOL_TOK):
      

      TOKEN lookahead = peekll3();
      switch (lookahead.type) {
        case (SC):  // a ";" follows an identifier in a variable declaration
          return var_decl();
        case (LPAR):  // a "(" follows an identifier in a function declaration
          return fun_decl();
      }

  }
  std::cerr << "expected decl() but got " << CurTok.lexeme << std::endl;
  exit(0);
}


// var_decl -> var_type IDENT ";"
std::unique_ptr<var_decl_ast> var_decl() {
  TOKEN type = var_type();
  TOKEN name = CurTok;
  auto variable_declaration = std::make_unique<var_decl_ast>(type, name);
  match(IDENT); //consume identifier  
  match(SC);  //consume ;

  return std::move(variable_declaration);
}

std::unique_ptr<function_ast> fun_decl() {
  auto type = type_spec();
  TOKEN identifier = CurTok;


  match(IDENT); // consume identifier
  match(LPAR);  // consumer identifier
  std::vector<std::unique_ptr<ast_node>> parameters = params();

  match(RPAR);

  // build function definition
  auto scope = block();
  auto prototype = std::make_unique<prototype_ast>(type, identifier, std::move(parameters));
  auto function = std::make_unique<function_ast>(std::move(prototype), std::move(scope));

  return std::move(function);

}

TOKEN var_type() {    
  switch (CurTok.type) {
    case (INT_TOK):
    case (FLOAT_TOK):
    case (BOOL_TOK):
      TOKEN type = CurTok;
      getNextToken();  //consume type token
      return type;
  }
  std::cerr << "Expected extern or int or float or bool or void" << std::endl;
  exit(0);                           
}

TOKEN type_spec() { 
  if (CurTok.type == VOID_TOK) {
    TOKEN type = CurTok;
    getNextToken();  //consume type token
    return type;
  }
  return var_type();
}


std::vector<std::unique_ptr<ast_node>> params() {
  
  std::vector<std::unique_ptr<ast_node>> parameter_list;
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
    match(VOID_TOK);
    parameter_list.push_back(
      std::move(
        std::make_unique<void_ast_node>(tok)
      )
    );
    return std::move(parameter_list);
  }
  //error
  std::cerr << "expected params()" << std::endl;
  exit(0);
}


void param_list(std::vector<std::unique_ptr<ast_node>>& list) {
  auto parameter = param();
  list.push_back(std::move(parameter));
  param_list_prime(list);
}

void param_list_prime(std::vector<std::unique_ptr<ast_node>>& list) {
  if (CurTok.type != RPAR) {
    match(COMMA);
    auto parameter = param();
    list.push_back(std::move(parameter));
    if (CurTok.type == COMMA) {
      param_list_prime(list);
    }
  }
}


std::unique_ptr<ast_node> param() {
  TOKEN type = var_type();
  TOKEN name = CurTok; 
  match(IDENT);
  auto parameter = std::make_unique<var_decl_ast>(type, name);
  return std::move(parameter);
}


std::unique_ptr<ast_node> block() {
  match(LBRA);
  std::vector<std::unique_ptr<ast_node>> local_declaration_list;
  std::vector<std::unique_ptr<ast_node>> statement_list;

  //build lists
  local_decls(local_declaration_list);
  stmt_list(statement_list);

  //make copies of the lists
  // auto local_declarations = local_declaration_list;
  // auto statements = statement_list;

  auto scope = std::make_unique<scope_ast>(std::move(local_declaration_list), std::move(statement_list));

  match(RBRA);

  return std::move(scope);
}


void local_decls(std::vector<std::unique_ptr<ast_node>>& list) {
  local_decls_prime(list);
}


void local_decls_prime(std::vector<std::unique_ptr<ast_node>>& list) {

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


std::unique_ptr<ast_node> local_decl() { 
  TOKEN type = var_type();
  TOKEN identifier = CurTok;
  match(IDENT);
  auto variable_declaration = std::make_unique<var_decl_ast>(type, identifier);
  match(SC);
  return std::move(variable_declaration);
}

void stmt_list(std::vector<std::unique_ptr<ast_node>>& list) {
  stmt_list_prime(list);
}

void stmt_list_prime(std::vector<std::unique_ptr<ast_node>>& list) {
  if (CurTok.type != RBRA) {
    auto statement = stmt();
    list.push_back(std::move(statement));
    stmt_list(list);
  }
}

std::unique_ptr<ast_node> stmt() {
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
  std::cerr << "Expected stmt" << std::endl;
  exit(0);
  
}

std::unique_ptr<ast_node> expr_stmt() {
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
      match(SC);
      return std::move(expression);
  }
  match(SC); 
  return nullptr;
}

std::unique_ptr<ast_node> while_stmt() {
  match(WHILE);
  match(LPAR);
  auto expression = expr();
  match(RPAR);
  auto statement = stmt();

  auto while_statement = std::make_unique<while_ast>(std::move(expression), std::move(statement));

  return std::move(while_statement);
}

std::unique_ptr<ast_node> if_stmt() {
  match(IF);
  match(LPAR);
  auto expression = expr();
  match(RPAR);
  auto if_block = block();
  auto else_statement = else_stmt();

  auto if_statement = std::make_unique<if_ast>(
    std::move(expression), std::move(if_block), std::move(else_statement)
  );

  return std::move(if_statement);
}

std::unique_ptr<ast_node> else_stmt() {
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
  match(ELSE);
  auto else_block = block();
  return std::move(else_block);
}

std::unique_ptr<ast_node> return_stmt() {
  match(RETURN);
  auto return_body = return_stmt_B();
  auto return_statement = std::make_unique<return_ast>(std::move(return_body));


  return std::move(return_statement);
}

std::unique_ptr<ast_node> return_stmt_B() {
  switch (CurTok.type) {
    case (NOT):
    case (LPAR):
    case (MINUS):
    case (BOOL_LIT):
    case (FLOAT_LIT):
    case (IDENT):
    case (INT_LIT):
      auto expression = expr();
      match(SC);
      return std::move(expression);
  }
  match(SC);
  return nullptr;
}


TOKEN peekll2() {
  TOKEN old = CurTok;
  TOKEN lookahead = getNextToken();
  TOKEN temp = lookahead; //token to return for lookahead
  putBackToken(lookahead);
  CurTok = old;
  return temp;
}


//will need a lookaheads due to IDENT
std::unique_ptr<ast_node> expr() {
  //determine if it is a variable assignment or a potential variable statement or function call
  if (CurTok.type == IDENT) {
      
      TOKEN lookahead = peekll2();
      if (lookahead.type == ASSIGN) {
        TOKEN name = CurTok;

        match(IDENT);
        match(ASSIGN);

        auto expression = expr();
        
        auto variable_assignment = std::make_unique<var_assign_ast>(name, std::move(expression));

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


  std::cerr << "expected a variable assignment or expression" << std::endl;
  exit(0); 
}

std::unique_ptr<ast_node> or_val() {
  auto lhs = and_val();
  while (CurTok.type == OR) {
    TOKEN op = CurTok;
    getNextToken();
    auto rhs = and_val();
    lhs = std::make_unique<binary_expr_ast>(op ,std::move(lhs), std::move(rhs));
  }
  return or_val_prime(std::move(lhs)); 
}

std::unique_ptr<ast_node> or_val_prime(std::unique_ptr<ast_node> lhs) {
  TOKEN op;
  
  switch (CurTok.type) {
  case(RPAR):
  case(SC):
  case(COMMA):
    return std::move(lhs);
  case (OR):
    op = CurTok;
    getNextToken();

    auto rhs = and_val();
      rhs = or_val_prime(std::move(rhs));
      auto bin_op = std::make_unique<binary_expr_ast>(
        op,
        std::move(lhs),
        std::move(rhs)
      );
      return std::move(bin_op);
  }
  //error
  std::cerr << "expected expression" << std::endl;
  exit(0);

}

std::unique_ptr<ast_node> and_val() {
  
  auto lhs = eq_val();
  while (CurTok.type == AND) {
    TOKEN op = CurTok;
    getNextToken();
    auto rhs = eq_val();
    lhs = std::make_unique<binary_expr_ast>(op ,std::move(lhs), std::move(rhs));
  }
  return and_val_prime(std::move(lhs));
}

std::unique_ptr<ast_node> and_val_prime(std::unique_ptr<ast_node> lhs) {
  TOKEN op;
  switch (CurTok.type) {
  case(RPAR):
  case(SC):
  case(OR):
  case(COMMA):
    return std::move(lhs);
  case (AND):
    op = CurTok;
    getNextToken();

    auto rhs = eq_val();
      rhs = and_val_prime(std::move(rhs));

      auto bin_op = std::make_unique<binary_expr_ast>(
        op,
        std::move(lhs),
        std::move(rhs)
      );
      return std::move(bin_op);
  }
  std::cerr << "and_val" << std::endl;
  exit(0);
}

std::unique_ptr<ast_node> eq_val() {
  auto lhs = comp_val();

  while (CurTok.type == EQ || CurTok.type == NE) {
    TOKEN op = CurTok;
    getNextToken();
    auto rhs = comp_val();
    lhs = std::make_unique<binary_expr_ast>(op ,std::move(lhs), std::move(rhs));
  }

  return eq_val_prime(std::move(lhs)); 
}

std::unique_ptr<ast_node> eq_val_prime(std::unique_ptr<ast_node> lhs) {
  TOKEN op;
  switch (CurTok.type) {
    case(AND):
    case(RPAR):
    case(SC):
    case(OR):
    case(COMMA):
      return std::move(lhs);
    case (EQ):
    case (NE):
      op = CurTok;
      getNextToken();

      auto rhs = comp_val();
      rhs = eq_val_prime(std::move(rhs));



      auto bin_op = std::make_unique<binary_expr_ast>(
        op,
        std::move(lhs),
        std::move(rhs)
      );
      return std::move(bin_op);
  }
  std::cerr << "eq_val" << std::endl;
  exit(0);
}

std::unique_ptr<ast_node> comp_val() { 
  auto lhs = add_val();
  while (CurTok.type == LE || CurTok.type == LT || CurTok.type == GE || CurTok.type == GT) {
    TOKEN op = CurTok;
    getNextToken();
    auto rhs = add_val();
    lhs = std::make_unique<binary_expr_ast>(op ,std::move(lhs), std::move(rhs));
  }
  return comp_val_prime(std::move(lhs));
}

std::unique_ptr<ast_node> comp_val_prime(std::unique_ptr<ast_node> lhs) {
  TOKEN op;
  switch (CurTok.type) {
  case(NE):
  case(AND):
  case(RPAR):      
  case(SC):
  case(EQ):
  case(OR):
  case(COMMA):
    return std::move(lhs);
  case (LE):
  case (LT):
  case (GE):
  case (GT):
    op = CurTok;
    getNextToken();

    auto rhs = add_val();
      rhs = comp_val_prime(std::move(rhs));



      auto bin_op = std::make_unique<binary_expr_ast>(
        op,
        std::move(lhs),
        std::move(rhs)
      );
      return std::move(bin_op);
  }
  std::cerr << "comp_val" << std::endl;
  exit(0);
}

std::unique_ptr<ast_node> add_val() {
  auto lhs = mul_val();

  while (CurTok.type == PLUS || CurTok.type == MINUS) {
    TOKEN op = CurTok;
    getNextToken();
    auto rhs = mul_val();
    lhs = std::make_unique<binary_expr_ast>(op ,std::move(lhs), std::move(rhs));
    
  }
  return add_val_prime(std::move(lhs));

  
}

std::unique_ptr<ast_node> 
add_val_prime(std::unique_ptr<ast_node> lhs) {
  TOKEN op;
  switch (CurTok.type) {
    case(NE):
    case(AND):
    case(RPAR):
    case(SC):
    case(LT):
    case(LE):
    case(EQ):
    case(GT):
    case(GE):
    case(OR):
    case(COMMA):
      return std::move(lhs);
    case (PLUS):
    case (MINUS):
      op = CurTok;
      getNextToken();
      auto rhs = mul_val();
      std::cout << ((rhs) ? rhs->to_string(0) : "null") << std::endl;
      rhs = add_val_prime(std::move(rhs));
      std::cout << ((rhs) ? rhs->to_string(0) : "null") << std::endl;

      std::cout << ((lhs) ? lhs->to_string(0) : "null") << std::endl;

      auto bin_op = std::make_unique<binary_expr_ast>(
        op,
        std::move(lhs),
        std::move(rhs)
      );
      return std::move(bin_op);
  }
  std::cerr << "add_val" << std::endl;
  exit(0);
}

std::unique_ptr<ast_node> mul_val() {
  auto lhs = unary();
  while (CurTok.type == ASTERIX || CurTok.type == DIV || CurTok.type == MOD) {
    TOKEN op = CurTok;
    getNextToken();
    auto rhs = unary();
    lhs = std::make_unique<binary_expr_ast>(op ,std::move(lhs), std::move(rhs));
  }
  return mul_val_prime(std::move(lhs));
}

std::unique_ptr<ast_node> mul_val_prime(std::unique_ptr<ast_node> lhs) {
  TOKEN op;
  
  switch (CurTok.type) {
    case(NE):
    case(AND):
    case(RPAR):
    case(PLUS):
    case(MINUS):
    case(SC):
    case(LT):
    case(LE):
    case(EQ):
    case(GT):
    case(GE):
    case(OR):
    case(COMMA):
      return std::move(lhs);
    case (ASTERIX):
    case (DIV):
    case (MOD):
      op = CurTok;
      getNextToken();
      auto rhs = unary();
      rhs = mul_val_prime(std::move(rhs));
      auto bin_op = std::make_unique<binary_expr_ast>(
        op,
        std::move(lhs),
        std::move(rhs)
      );
      return std::move(bin_op);
  }
  std::cerr << "mul_val" << std::endl;
  exit(0);
}

std::unique_ptr<ast_node> unary() {
  switch (CurTok.type) {
  case (LPAR):
  case (BOOL_LIT):
  case (FLOAT_LIT):
  case (IDENT):
  case (INT_LIT):   
  {
    auto identifier = identifiers();
    return std::move(identifier);
  }
  case (NOT):
  case (MINUS):
    TOKEN op = CurTok;
    getNextToken();
    auto unary_expression = std::make_unique<unary_expr_ast>(op, std::move(unary()));
    
    return std::move(unary_expression);
  }
  std::cerr << "unary" << std::endl;
  exit(0);
}

std::unique_ptr<ast_node> identifiers() {
  switch (CurTok.type) {
    case (IDENT): 
    {
      auto identifier = identifiers_B();
      return std::move(identifier); 
    }
    case (INT_LIT):
    case (FLOAT_LIT):
    case (BOOL_LIT): {
      TOKEN tok = CurTok;
      getNextToken();
      auto result = std::make_unique<literal_ast_node>(tok);
      return std::move(result);    
    } case (LPAR): 
      match(LPAR);
      auto expression = expr();
      match(RPAR);      
      return std::move(expression);
  }

  std::cerr << "expected identifier" << std::endl;
  exit(0);
}

std::unique_ptr<ast_node> identifiers_B() {
  TOKEN identifier = CurTok;
  getNextToken();
  switch (CurTok.type) {
    case(NE):
    case(AND):
    case(RPAR):
    case(PLUS):
    case(MINUS):
    case(SC):
    case(LT):
    case(LE):
    case(EQ):
    case(GT):
    case(GE):
    case(OR):
    case(COMMA):
    case(ASTERIX):
    case(DIV):
    case(MOD): {
      auto result = std::make_unique<literal_ast_node>(identifier);
      return std::move(result);  
    } case (LPAR):
      // function call
      TOKEN callee = identifier;

      match(LPAR);
      std::vector<std::unique_ptr<ast_node>> arguments = args();
      
      match(RPAR);
      auto function_call = std::make_unique<call_expr_ast>(callee, std::move(arguments));
      return std::move(function_call);
  } 

  std::cerr << "expected (" << std::endl;
  exit(0);
}

std::vector<std::unique_ptr<ast_node>> args() {
  std::vector<std::unique_ptr<ast_node>> argument_list;
  // if not the follow case, we will populate the list otherwise we simply return an empty list to show we have no args
  if (CurTok.type != RPAR) {
    arg_list(argument_list);
  }

  return std::move(argument_list);
}

void arg_list(std::vector<std::unique_ptr<ast_node>>& list) {
  auto expression = expr();
  list.push_back(std::move(expression));
  
  arg_list_prime(list);

  
}

void arg_list_prime(std::vector<std::unique_ptr<ast_node>>& list) {
  if (CurTok.type != RPAR) {
    match(COMMA);
    auto expression = expr();
    list.push_back(std::move(expression));
    arg_list_prime(list);
  }
}




//===----------------------------------------------------------------------===//
// AST nodes
//===----------------------------------------------------------------------===//

/// ast_node - Base class for all AST nodes.
class ast_node {
public:
  virtual ~ast_node() {}
  virtual Value *codegen() = 0;
  virtual std::string to_string(int depth) const {
    return "";
  };
};




/// literal_ast_node - class to store integer, float or boolean literals or indentifiers
class literal_ast_node : public ast_node {
  TOKEN Tok;
  

public:
  literal_ast_node(TOKEN tok) : Tok(tok) {}
  virtual Value *codegen() override {
    switch (Tok.type) {
      case (INT_TOK):
        return ConstantInt::get(TheContext, APInt(std::stoi(Tok.lexeme), false));
      case (FLOAT_TOK):
        return ConstantFP::get(TheContext, APFloat(std::stof(Tok.lexeme)));
      case (BOOL_TOK):
        return ConstantInt::get(TheContext, APInt(std::stoi(Tok.lexeme), false));
      case (IDENT):
        return;
        //for later

    }


  };
  virtual std::string to_string(int depth) const override {
  // return a sting representation of this AST node
    std::string whitespace(depth, ' ');
    return whitespace + Tok.lexeme;
  };
};


/// void_ast_node - Class for to store void token, mainly used in function definitions and declarations
class void_ast_node : public ast_node {
  TOKEN Tok;

public:
  void_ast_node(TOKEN tok) : Tok(tok) {}
  virtual Value *codegen() override;
  virtual std::string to_string(int depth) const override {
  // return a sting representation of this AST node
    std::string whitespace(depth, ' ');
    return whitespace + Tok.lexeme;
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
    virtual Value *codegen() override {
      

      switch (Op.type) {
        case (PLUS):
          return Builder.CreateAdd(LHS->codegen(), RHS->codegen());
        case (MINUS):
          return Builder.CreateSub(LHS->codegen(), RHS->codegen());
        case (ASTERIX):
          return Builder.CreateMul(LHS->codegen(), RHS->codegen());
        case (DIV):
          return Builder.CreateSDiv(LHS->codegen(), RHS->codegen());
        case (MOD):
          // return Builder.createMod(LHS->codegen(), RHS->codegen());


        case(EQ):
          
        case(NE):
        case(LE):
        case(LT):
        case(GE):
        case(GT):
      }
    };

    virtual std::string to_string(int depth) const override {
    // return a sting representation of this AST node
      
      std::string whitespace(depth, ' ');
      std::string left = (LHS) ? LHS->to_string(depth+1) : "null";
      std::string right = (RHS) ? RHS->to_string(depth+1) : "null";
      return whitespace + "Op: " + Op.lexeme + "\n" + whitespace + left + "\n" + whitespace + right; 
    };

};



// class for unary expressions
class unary_expr_ast : public ast_node {
  TOKEN Op;
  std::unique_ptr<ast_node> Expr;

public:
  unary_expr_ast(TOKEN op, std::unique_ptr<ast_node> expr) : Op(op), Expr(std::move(expr)) {}

  virtual Value *codegen() override {
    switch (Op.type) {
      case (NOT):
        return Builder.CreateNot(Expr->codegen());
      case (MINUS):
        return Builder.CreateNeg(Expr->codegen());

    }
  };
  
  virtual std::string to_string(int depth) const override {
    // return a sting representation of this AST node
    std::string whitespace(depth, ' ');
    depth += 1;
    std::string expression = (Expr) ? Expr->to_string(depth+1) : "null";
    return whitespace + "Op: " + Op.lexeme + "\n" +  expression;
  };
};

// class for function calls
class call_expr_ast : public ast_node {
    TOKEN Callee;
    std::vector<std::unique_ptr<ast_node>> Args;

public:
  call_expr_ast(TOKEN callee,std::vector<std::unique_ptr<ast_node>> args) :
    Callee(callee), Args(std::move(args)) {}

  virtual Value *codegen() override {
    Function *func_callee = TheModule -> getFunction(Callee.lexeme);

    // various error handling will go here

    std::vector<Value *> args_v;


    for(auto v: Args) {
      args_v.push_back(v->codegen());
    }

    return Builder.CreateCall(func_callee, args_v);
  };

  virtual std::string to_string(int depth) const override {
    std::string arguments = "";
    for (int i = 0; i < Args.size(); i++) {
      std::string element = (Args[i]) ? Args[i]->to_string(depth+1) : "null";
      (i < Args.size() - 1) ? arguments += element + ", ": arguments += element;
    }
    std::string whitespace(depth, ' ');
    return whitespace + Callee.lexeme + "(" + arguments + ")";
  };
};


//class for function signature (used for for both the function definition and externs)
class prototype_ast : public ast_node {
    TOKEN Type;
    TOKEN Name;
    std::vector<std::unique_ptr<ast_node>> Args;

public:
  prototype_ast(TOKEN type, TOKEN name, std::vector<std::unique_ptr<ast_node>> args) 
    : Type(type), Name(name), Args(std::move(args)) {}

  virtual Value *codegen() override {
    
  };

  virtual std::string to_string(int depth) const override {
    std::string arguments = "";
    for (int i = 0; i < Args.size(); i++) {
      std::string element = (Args[i]) ? Args[i]->to_string(0) : "null";
      (i < Args.size() - 1) ? arguments += element + ", ": arguments += element;
    }
    std::string whitespace(depth, ' ');
    return whitespace + Type.lexeme + " " + Name.lexeme + "(" + arguments + ")";
  };
};


//class for function signature and body
class function_ast : public ast_node  {
    std::unique_ptr<prototype_ast> Proto;
    std::unique_ptr<ast_node> Body;

public:
  function_ast(std::unique_ptr<prototype_ast> proto, 
    std::unique_ptr<ast_node> body) : Proto(std::move(proto)), Body(std::move(body)) {}
  virtual std::string to_string(int depth) const override {
    // return a sting representation of this AST node
    std::string whitespace(depth, ' ');
    std::string prototype = (Proto) ? Proto->to_string(0) : "null";
    std::string functionbody = (Body) ? Body->to_string(depth+1) : "null";
    return whitespace + prototype + "\n" + functionbody; 
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
  
  virtual std::string to_string(int depth) const override {
    // return a sting representation of this AST node
    std::string con = (Condition) ? Condition->to_string(0) : "null";
    std::string ifbody = (If_body) ? If_body->to_string(depth+1) : "null";
    std::string elsebody = (Else_body) ? Else_body->to_string(depth+1) : "null";
    std::string whitespace(depth, ' ');
    return whitespace + "if (" + con + ")\n " + ifbody + "\n" + whitespace +"else \n" + whitespace + elsebody; 
  };
};

// class for while statement structure
class while_ast : public ast_node {
    std::unique_ptr<ast_node> Condition;
    std::unique_ptr<ast_node> Body;
    
public:
  while_ast(std::unique_ptr<ast_node> condition, std::unique_ptr<ast_node> body) :
    Condition(std::move(condition)), Body(std::move(body)) {}
  
  virtual std::string to_string(int depth) const override {
    // return a sting representation of this AST node
    std::string con = (Condition) ? Condition->to_string(0) : "null";
    std::string whilebody = (Body) ? Body->to_string(depth+1) : "null";
    std::string whitespace(depth, ' ');
    return whitespace + "while (" + con + ") \n" + whilebody; 
    
  };
};

//class for return statement structure
class return_ast : public ast_node {
  std::unique_ptr<ast_node> Body;

public:
  return_ast(std::unique_ptr<ast_node> body) : Body(std::move(body)) {}
  virtual std::string to_string(int depth) const override {
    // return a sting representation of this AST node
    std::string returnbody = (Body) ? Body->to_string(0) : "null";
    std::string whitespace(depth, ' ');
    return whitespace + "return " + returnbody;
  };
};

//class for variable declaration structure
class var_decl_ast : public ast_node {
  TOKEN Type;
  TOKEN Name;

public:
  var_decl_ast(TOKEN type, TOKEN name) : Type(type), Name(name)  {}
  virtual std::string to_string(int depth) const override {
    // return a sting representation of this AST node
    std::string whitespace(depth, ' ');
    return whitespace + Type.lexeme + " " + Name.lexeme;
  };
};

// class for variable assignment structure
class var_assign_ast : public ast_node {
  TOKEN Name;
  std::unique_ptr<ast_node> Expr;

public:
  var_assign_ast(TOKEN name, std::unique_ptr<ast_node> expr) : Name(name), Expr(std::move(expr)) {

  }
  virtual std::string to_string(int depth) const override {
    // return a sting representation of this AST node
    std::string expression = (Expr) ? Expr->to_string(depth+1) : "null";
    std::string whitespace(depth, ' ');
    return whitespace + Name.lexeme + " assigned\n" + expression;
  };
};


class scope_ast : public ast_node {
  std::vector<std::unique_ptr<ast_node>> List_a;
  std::vector<std::unique_ptr<ast_node>> List_b;

public:
  scope_ast(std::vector<std::unique_ptr<ast_node>> list_a, std::vector<std::unique_ptr<ast_node>> list_b) : 
    List_a(std::move(list_a)), List_b(std::move(list_b)) {}
  virtual std::string to_string(int depth) const override {
    // return a sting representation of this AST node
    std::string whitespace(depth, ' ');
    std::string list_elements = "";
    int cur_depth = depth;
    for (int i = 0; i < List_a.size(); i++) {
      std::string element = (List_a[i]) ? List_a[i]->to_string(cur_depth) : "null";
      (i < List_a.size() - 1) ? list_elements += element + "\n": list_elements += element;
    }
    list_elements += "\n";
    for (int i = 0; i < List_b.size(); i++) {
      std::string element = (List_b[i]) ? List_b[i]->to_string(cur_depth) : "null";
      (i < List_b.size() - 1) ? list_elements += element + "\n": list_elements += element;
    }
    return list_elements;
  };
};





//===----------------------------------------------------------------------===//
// Code Generation
//===----------------------------------------------------------------------===//

LLVMContext TheContext;
IRBuilder<> Builder(TheContext);
std::unique_ptr<Module> TheModule;

// //===----------------------------------------------------------------------===//
// // AST Printer
// //===----------------------------------------------------------------------===//

inline llvm::raw_ostream &operator<<(llvm::raw_ostream &os,
                                     const ast_node &ast) {
  os << ast.to_string(0);
  return os;
}

//===----------------------------------------------------------------------===//
// Main driver code.
//===----------------------------------------------------------------------===//

int main(int argc, char **argv) {
  if (argc == 2) {
    pFile = fopen(argv[1], "r");
    if (pFile == NULL)
      perror("Error opening file");
  } else {
    std::cout << "Usage: ./code InputFile\n";
    return 1;
  }

  // initialize line number and column numbers to zero
  lineNo = 1;
  columnNo = 1;

  // get the first token
  getNextToken();
  // while (CurTok.type != EOF_TOK) {
  //   fprintf(stderr, "Token: %s with type %d\n", CurTok.lexeme.c_str(),
  //           CurTok.type);
  //   getNextToken();
  // }
  // fprintf(stderr, "Lexer Finished\n");

  // Make the module, which holds all the code.
  TheModule = std::make_unique<Module>("mini-c", TheContext);

  // Run the parser now.
  auto test = parser();

  std::cout << test->to_string(0) << std::endl;
  fprintf(stderr, "Parsing Finished\n");

  //********************* Start printing final IR **************************
  // Print out all of the generated code into a file called output.ll
  auto Filename = "output.ll";
  std::error_code EC;
  raw_fd_ostream dest(Filename, EC, sys::fs::OF_None);

  if (EC) {
    errs() << "Could not open file: " << EC.message();
    return 1;
  }
  // TheModule->print(errs(), nullptr); // print IR to terminal
  TheModule->print(dest, nullptr);
  //********************* End printing final IR ****************************

  fclose(pFile); // close the file that contains the code that was parsed
  return 0;
}