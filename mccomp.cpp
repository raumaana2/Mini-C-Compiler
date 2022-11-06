#include "mccomp.hpp"

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
    std::cout << "matched " << CurTok.type << std::endl;
    getNextToken();
  } else {
    std::cerr << "Expected token " << word << " but got " << CurTok.type << std::endl;
    exit(0);
  }

}

//===----------------------------------------------------------------------===//
// Recursive Descent Parser - Function call for each production
//===----------------------------------------------------------------------===//


void parser() {
  // add body
  
  auto program_scope = program();

  
  switch (CurTok.type) {
    case (EOF_TOK):
      std::cout << "Parsed correctly " << std::endl;
    default:
      //error
      exit(0);
  }
  
}


// program ::= extern_list decl_list
std::unique_ptr<ast_node> program() {
  std::vector<std::unique_ptr<ast_node>> extern_vector;
  std::vector<std::unique_ptr<ast_node>> decl_vector;

  if (CurTok.type == EXTERN) {
    extern_list(std::move(extern_vector));
    decl_list(std::move(decl_vector));
  } else if (CurTok.type == BOOL_TOK || FLOAT_TOK || INT_TOK || VOID_TOK)  {
    decl_list(std::move(decl_vector));
  } else {
  
    std::cout << "Expected extern or bool or float or int or void " << std::endl;

    exit(0);
  }


  auto scope = std::make_unique<scope_ast>(std::move(extern_vector), std::move(decl_vector));

  return std::move(scope);
}


void extern_list(std::vector<std::unique_ptr<ast_node>> list) {
  auto _extern_ = extern_();
  list.push_back(std::move(_extern_));
  extern_list_prime(std::move(list));
}

void extern_list_prime(std::vector<std::unique_ptr<ast_node>> list) {

  switch (CurTok.type) {
    case (INT_TOK):
    case (FLOAT_TOK):
    case (BOOL_TOK):
    case (VOID_TOK):
      return;
    case (EXTERN):
      auto _extern_ = extern_();
      list.push_back(std::move(_extern_));
      extern_list_prime(std::move(list));
      return;
  }

  std::cerr << "Expected extern or int or float or bool or void" << std::endl;
  exit(0);
}

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



void decl_list(std::vector<std::unique_ptr<ast_node>> list) {

  auto declaration = decl();
  list.push_back(std::move(declaration));
  decl_list_prime(std::move(list));
}

void decl_list_prime(std::vector<std::unique_ptr<ast_node>> list) {
  if (CurTok.type != EOF_TOK) {
    auto declaration = decl();
    list.push_back(std::move(declaration));
    decl_list_prime(std::move(list));
  }
}


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


std::unique_ptr<ast_node> decl() { 
  switch (CurTok.type) {
    case (VOID_TOK):
      return fun_decl();
    case (INT_TOK):
    case (FLOAT_TOK):
    case (BOOL_TOK):
      // lookahead 
      TOKEN lookahead = peekll3();
      switch (lookahead.type) {
        case (COMMA):
          return var_decl();
        case (LPAR):
          return fun_decl();
      }

  }
  exit(0);
}

std::unique_ptr<var_decl_ast> var_decl() {
  TOKEN type = var_type();
  TOKEN name = CurTok;
  auto variable_declaration = std::make_unique<var_decl_ast>(type, name);
  match(IDENT); //consume identifier  
  match(SC);  //consume ;

  return variable_declaration;
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
    
    param_list(std::move(parameter_list));
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
  exit(0);
}


void param_list(std::vector<std::unique_ptr<ast_node>> list) {
  auto parameter = param();
  list.push_back(std::move(parameter));
  param_list_prime(std::move(list));
}

void param_list_prime(std::vector<std::unique_ptr<ast_node>> list) {
  if (CurTok.type != RPAR) {
    match(COMMA);
    auto parameter = param();
    list.push_back(std::move(parameter));
    if (CurTok.type == COMMA) {
      param_list_prime(std::move(list));
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
  local_decls(std::move(local_declaration_list));
  stmt_list(std::move(statement_list));

  //make copies of the lists
  // auto local_declarations = local_declaration_list;
  // auto statements = statement_list;

  auto scope = std::make_unique<scope_ast>(std::move(local_declaration_list), std::move(statement_list));

  match(RBRA);

  return std::move(scope);
}


void local_decls(std::vector<std::unique_ptr<ast_node>> list) {
  local_decls_prime(std::move(list));
}


void local_decls_prime(std::vector<std::unique_ptr<ast_node>> list) {

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
  local_decls(std::move(list));
}


std::unique_ptr<ast_node> local_decl() { 
  TOKEN type = var_type();
  TOKEN identifier = CurTok;
  match(IDENT);
  auto variable_declaration = std::make_unique<var_decl_ast>(type, identifier);
  match(SC);
  return std::move(variable_declaration);
}

void stmt_list(std::vector<std::unique_ptr<ast_node>> list) {
  stmt_list_prime(std::move(list));
}

void stmt_list_prime(std::vector<std::unique_ptr<ast_node>> list) {
  if (CurTok.type != RBRA) {
    auto statement = stmt();
    list.push_back(std::move(statement));
    stmt_list(std::move(list));
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

    return expr_stmt();
  case (LBRA):
    return block();
  case (IF):
    return if_stmt();
  case (WHILE):
    return while_stmt();
  case (RETURN):
    return return_stmt();
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

        auto variable_assignment = std::make_unique<var_assign_ast>(name, std::move(expr()));

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
      return or_val();
  }


  std::cerr << "expected a variable assignment or expression" << std::endl;
  exit(0); 
}

std::unique_ptr<ast_node> or_val() {
  auto lhs = and_val();

  lhs = or_val_prime(std::move(lhs)); 

  return std::move(lhs); 
}

std::unique_ptr<ast_node> or_val_prime(std::unique_ptr<ast_node> lhs) {
  TOKEN op;
  
  switch (CurTok.type) {
  case(RPAR):
  case(SC):
  case(COMMA):
    return nullptr;
  case (OR):
    op = CurTok;
    getNextToken();

    auto lhs_prime = and_val();
    
    auto rhs = or_val_prime(std::move(lhs));

    return std::move(
      std::make_unique<binary_expr_ast>(
        op,
        std::move(lhs_prime),
        std::move(rhs)
      )
    );
  }
  //error
  exit(0);

}

std::unique_ptr<ast_node> and_val() {
  
  auto lhs = eq_val();
  lhs = and_val_prime(std::move(lhs));
  return std::move(lhs);
}

std::unique_ptr<ast_node> and_val_prime(std::unique_ptr<ast_node> lhs) {
  TOKEN op;
  switch (CurTok.type) {
  case(RPAR):
  case(SC):
  case(OR):
  case(COMMA):
    return nullptr;
  case (AND):
    op = CurTok;
    getNextToken();

    auto lhs_prime = eq_val();
    
    auto rhs = and_val_prime(std::move(lhs));

    return std::move(
      std::make_unique<binary_expr_ast>(
        op,
        std::move(lhs_prime),
        std::move(rhs)
      )
    );
  }
  exit(0);
}

std::unique_ptr<ast_node> eq_val() {
  auto lhs = comp_val();
  lhs = eq_val_prime(std::move(lhs));
  return std::move(lhs); 
}

std::unique_ptr<ast_node> eq_val_prime(std::unique_ptr<ast_node> lhs) {
  TOKEN op;
  switch (CurTok.type) {
    case(AND):
    case(RPAR):
    case(SC):
    case(OR):
    case(COMMA):
      return nullptr;
    case (EQ):
    case (NE):
      op = CurTok;
      getNextToken();

      auto lhs_prime = comp_val();
      
      auto rhs = eq_val_prime(std::move(lhs));

      return std::move(
        std::make_unique<binary_expr_ast>(
          op,
          std::move(lhs_prime),
          std::move(rhs)
        )
      );
  }
  exit(0);
}

std::unique_ptr<ast_node> comp_val() { 
  auto lhs = add_val();
  lhs = comp_val_prime(std::move(lhs));

  return std::move(lhs);
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
    return nullptr;
  case (LE):
  case (LT):
  case (GE):
  case (GT):
    op = CurTok;
    getNextToken();

    auto lhs_prime = add_val();
    
    auto rhs = comp_val_prime(std::move(lhs));

    return std::move(
      std::make_unique<binary_expr_ast>(
        op,
        std::move(lhs_prime),
        std::move(rhs)
      )
    );
  }
  exit(0);
}

std::unique_ptr<ast_node> add_val() {
  auto lhs = mul_val();
  lhs = add_val_prime(std::move(lhs));

  return std::move(lhs);
}

std::unique_ptr<ast_node> add_val_prime(std::unique_ptr<ast_node> lhs) {
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
      return nullptr;
    case (PLUS):
    case (MINUS):
      op = CurTok;
      getNextToken();

      auto lhs_prime = mul_val();
      
      auto rhs = add_val_prime(std::move(lhs));

      return std::move(
        std::make_unique<binary_expr_ast>(
          op,
          std::move(lhs_prime),
          std::move(rhs)
        )
      );
  }
  exit(0);
}

std::unique_ptr<ast_node> mul_val() {
  auto lhs = unary();
  lhs = mul_val_prime(std::move(lhs));
  return std::move(lhs);
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
      return nullptr;
    case (ASTERIX):
    case (DIV):
    case (MOD):
      op = CurTok;
      getNextToken();

      auto lhs_prime = unary();
    
      auto rhs = mul_val_prime(std::move(lhs));

      return std::move(
        std::make_unique<binary_expr_ast>(
          op,
          std::move(lhs_prime),
          std::move(rhs)
        )
      );
  }
  exit(0);
}

std::unique_ptr<ast_node> unary() {
  switch (CurTok.type) {
  case (LPAR):
  case (BOOL_LIT):
  case (FLOAT_LIT):
  case (IDENT):
  case (INT_LIT):
    return identifiers();
  case (NOT):
  case (MINUS):
    TOKEN op = CurTok;
    getNextToken();
    auto unary_expression = std::make_unique<unary_expr_ast>(op, unary());
    
    return std::move(unary_expression);
  }
  exit(0);
}

std::unique_ptr<ast_node> identifiers() {
  std::cout << CurTok.type << std::endl;
  switch (CurTok.type) {
    case (IDENT):
      return identifiers_B(); 
    case (INT_LIT):
    case (FLOAT_LIT):
    case (BOOL_LIT):
      TOKEN tok = CurTok;
      getNextToken();
      auto result = std::make_unique<literal_ast_node>(tok);
      return std::move(result);           
  }

  std::cerr << "expected identifier" << std::endl;
  exit(0);
}

std::unique_ptr<ast_node> identifiers_B() {
  std::cout << "in identifiers_B" << std::endl;
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
      TOKEN callee = CurTok;
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
    arg_list(std::move(argument_list));
  }

  return std::move(argument_list);
}

void arg_list(std::vector<std::unique_ptr<ast_node>> list) {
  auto expression = expr();
  list.push_back(std::move(expression));
  
  arg_list_prime(std::move(list));

  
}

void arg_list_prime(std::vector<std::unique_ptr<ast_node>> list) {
  if (CurTok.type != RPAR) {
    match(COMMA);
    auto expression = expr();
    list.push_back(std::move(expression));
    arg_list_prime(std::move(list));
  }
}








//===----------------------------------------------------------------------===//
// Code Generation
//===----------------------------------------------------------------------===//

static LLVMContext TheContext;
static IRBuilder<> Builder(TheContext);
static std::unique_ptr<Module> TheModule;

// //===----------------------------------------------------------------------===//
// // AST Printer
// //===----------------------------------------------------------------------===//

// inline llvm::raw_ostream &operator<<(llvm::raw_ostream &os,
//                                      const ast_node &ast) {
//   os << ast.to_string();
//   return os;
// }

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
  fprintf(stderr, "Lexer Finished\n");

  // Make the module, which holds all the code.
  TheModule = std::make_unique<Module>("mini-c", TheContext);

  // Run the parser now.
  parser();
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