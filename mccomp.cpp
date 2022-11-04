#include "llvm/ADT/APFloat.h"
#include "llvm/ADT/Optional.h"
#include "llvm/ADT/STLExtras.h"
#include "llvm/IR/BasicBlock.h"
#include "llvm/IR/Constants.h"
#include "llvm/IR/DerivedTypes.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/Instructions.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/LegacyPassManager.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Type.h"
#include "llvm/IR/Verifier.h"
#include "llvm/Support/FileSystem.h"
#include "llvm/Support/Host.h"
#include "llvm/Support/TargetRegistry.h"
#include "llvm/Support/TargetSelect.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Target/TargetMachine.h"
#include "llvm/Target/TargetOptions.h"
#include <algorithm>
#include <cassert>
#include <cctype>
#include <cstdio>
#include <cstdlib>
#include <iostream>
#include <map>
#include <memory>
#include <queue>
#include <string.h>
#include <string>
#include <system_error>
#include <utility>
#include <vector>


using namespace llvm;
using namespace llvm::sys;

FILE *pFile;


//===----------------------------------------------------------------------===//
// Lexer
//===----------------------------------------------------------------------===//

// The lexer returns one of these for known things.
enum TOKEN_TYPE {

  IDENT = -1,        // [a-zA-Z_][a-zA-Z_0-9]*
  ASSIGN = int('='), // '='

  // delimiters
  LBRA = int('{'),  // left brace
  RBRA = int('}'),  // right brace
  LPAR = int('('),  // left parenthesis
  RPAR = int(')'),  // right parenthesis
  SC = int(';'),    // semicolon
  COMMA = int(','), // comma

  // types
  INT_TOK = -2,   // "int"
  VOID_TOK = -3,  // "void"
  FLOAT_TOK = -4, // "float"
  BOOL_TOK = -5,  // "bool"

  // keywords
  EXTERN = -6,  // "extern"
  IF = -7,      // "if"
  ELSE = -8,    // "else"
  WHILE = -9,   // "while"
  RETURN = -10, // "return"
  // TRUE   = -12,     // "true"
  // FALSE   = -13,     // "false"

  // literals
  INT_LIT = -14,   // [0-9]+
  FLOAT_LIT = -15, // [0-9]+.[0-9]+
  BOOL_LIT = -16,  // "true" or "false" key words

  // logical operators
  AND = -17, // "&&"
  OR = -18,  // "||"

  // operators
  PLUS = int('+'),    // addition or unary plus
  MINUS = int('-'),   // substraction or unary negative
  ASTERIX = int('*'), // multiplication
  DIV = int('/'),     // division
  MOD = int('%'),     // modular
  NOT = int('!'),     // unary negation

  // comparison operators
  EQ = -19,      // equal
  NE = -20,      // not equal
  LE = -21,      // less than or equal to
  LT = int('<'), // less than
  GE = -23,      // greater than or equal to
  GT = int('>'), // greater than

  // special tokens
  EOF_TOK = 0, // signal end of file

  // invalid
  INVALID = -100 // signal invalid token
};

// TOKEN struct is used to keep track of information about a token
struct TOKEN {
  int type = -100;
  std::string lexeme;
  int lineNo;
  int columnNo;
};

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

/// cur_tok/get_next_token - Provide a simple token buffer.  cur_tok is the current
/// token the parser is looking at.  get_next_token reads another token from the
/// lexer and updates cur_tok with its results.
TOKEN cur_tok;
std::deque<TOKEN> tok_buffer;

TOKEN get_next_token() {

  if (tok_buffer.size() == 0)
    tok_buffer.push_back(gettok());

  TOKEN temp = tok_buffer.front();
  tok_buffer.pop_front();

  return cur_tok = temp;
}

void match(int word) {
  if (cur_tok.type == word) {
    get_next_token();
  }
  std::cerr << "Expected an identifier" << word << std::endl;
  exit(0);
}

void put_back_token(TOKEN tok) { tok_buffer.push_front(tok); }




//===----------------------------------------------------------------------===//
// Recursive Descent Parser - Function call for each production
//===----------------------------------------------------------------------===//

/* Add function calls for each production */

std::unique_ptr<ast_node> parser() {
  // add body
  auto program = program();
  
  switch (cur_tok.type) {
    case (EOF_TOK):
      return program;
    default:
      return nullptr;
  }
  
}

// program ::= extern_list decl_list
std::unique_ptr<ast_node> program() {
  switch (cur_tok.type) {
    case (EXTERN):
      extern_list();
      decl_list();
    case (BOOL_TOK):
    case (FLOAT_TOK):
    case (INT_TOK):
    case (VOID_TOK):
      decl_list();
  }
  //error
  exit(0);
}

std::vector<std::unique_ptr<ast_node>> extern_list_vector;

void extern_list() {
  auto _extern_ = extern_();
  extern_list_vector.push_back(_extern_);
  if (cur_tok.type = EXTERN) {
    extern_list();
  }
}

std::unique_ptr<ast_node> extern_() {
  match(EXTERN);
  auto type = type_spec();
  match(IDENT);
  match(LPAR);
  auto parameters = params();
  match(RPAR);
  match(SC);
  auto prototype = std::make_unique<prototype_ast>(type_spec, parameters);

  return std::move(prototype);
}


std::vector<std::unique_ptr<ast_node>> decl_list_vector;

void decl_list() {
  auto decleration = decl();
  decl_list_vector.push_back(decl());

  if (cur_tok.type == BOOL_TOK || FLOAT_TOK || INT_TOK || VOID_TOK) {
    decl_list();
  }

}

// std::unique_ptr<ast_node> decl_list_prime() {
//   switch (cur_tok.type) {
//     case (EOF_TOK):
//       return nullptr;
//   }
//   auto decleration = decl();
//   auto decleration_list = decl_list_prime();
  
// }


std::unique_ptr<ast_node> decl() { 
  switch (cur_tok.type) {
    case (VOID_TOK):
      return fun_decl();
    case (INT_TOK):
    case (FLOAT_TOK):
    case (BOOL_TOK):
      TOKEN old = cur_tok;
      TOKEN lookahead_1 = get_next_token();
      TOKEN lookahead_2 = get_next_token();
      switch (lookahead_2.type) {
        case (COMMA):
          put_back_token(lookahead_1);
          put_back_token(lookahead_2);
          cur_tok = old;
          return var_decl();
        case (LPAR):
          put_back_token(lookahead_1);
          put_back_token(lookahead_2);
          cur_tok = old;
          return fun_decl();
      }

  }
}

std::unique_ptr<ast_node> var_decl() {
  auto tok = cur_tok;
  auto variable_declaration = std::make_unique<var_decl_ast>(tok);
  match(IDENT);
  match(SC);

  return variable_declaration;
}

std::unique_ptr<ast_node> fun_decl() {
  auto type = type_spec();
  match(IDENT);
  match(LPAR); 
  
  params()
  match(RPAR)
  block();
}

std::unique_ptr<ast_node> var_type() {       
  switch(cur_tok.type) {
    case(INT_TOK):
      return 
    case(FLOAT_TOK):
      return
    case(BOOL_TOK):
      return std::make
  }                                             
}

std::unique_ptr<ast_node> type_spec() { 
  return match(VOID_TOK) || var_type();
}


std::unique_ptr<ast_node> params() {
  switch (cur_tok.type) {
  case (RPAR):
    return true;
  case (BOOL_TOK):
  case (FLOAT_TOK):
  case (INT_TOK):
    return param_list();
  case (VOID_TOK):
    return match(VOID_TOK);
  }
  return false;
}

std::unique_ptr<ast_node> param_list() {
  return param() && param_list_prime();
}

std::unique_ptr<ast_node> param_list_prime() {
  switch (cur_tok.type) {
  case (RPAR):
    return nullptr;
  case (COMMA):
    return match(COMMA) && param() && param_list_prime();
  }
  return false;
}

std::unique_ptr<ast_node> param() {
  return var_type() && match(IDENT); 
}

std::unique_ptr<ast_node> block() {
  return match(LBRA) && local_decls() && stmt_list() && match(RBRA);
}

std::unique_ptr<ast_node> local_decls() {
  switch (cur_tok.type) {
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
  return local_decls_prime(); 
}

std::unique_ptr<ast_node> local_decls_prime() {
  switch (cur_tok.type) {
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
      return true;
  }
  return local_decl() && local_decls_prime();
}

std::unique_ptr<ast_node> local_decl() { 
  return var_type() && match(IDENT) && match(SC);
}

std::unique_ptr<ast_node> stmt_list() { 
  if (cur_tok.type == RBRA) {
    return true;
  }
  return stmt_list_prime();
}

std::unique_ptr<ast_node> stmt_list_prime() {
  if (cur_tok.type == RBRA) {
    return true;
  }
  return stmt() && stmt_list_prime();
}

std::unique_ptr<ast_node> stmt() {
  switch (cur_tok.type) {
  case (NOT):
  case (LPAR):
  case (MINUS):
  case (SC):
  case (BOOL_LIT):
  case (FLOAT_LIT):
  case (IDENT):
  case (INT_LIT):
    get_next_token();
    return expr_stmt();
  case (LBRA):
    get_next_token();
    return block();
  case (IF):
    get_next_token();
    return if_stmt();
  case (WHILE):
    get_next_token();
    return while_stmt();
  case (RETURN):
    get_next_token();
    return return_stmt();
  }
  std::cerr << "Expected stmt" << std::endl;
  exit(0);
  
}

std::unique_ptr<ast_node> expr_stmt() {
  switch (cur_tok.type) {
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
      return expression;
  }
  match(SC); 
  return nullptr;
}

std::unique_ptr<ast_node> while_stmt() {
  return match(WHILE) && match(LPAR) && expr() && match(RPAR) && stmt();
}

std::unique_ptr<ast_node> if_stmt() {
  match(IF);
  match(LPAR);
  auto expression = expr();
  match(RPAR);
  auto if_block = block();
  auto else_statement = else_stmt;

  return std::make_unique<if_ast>(std::move(expression), std::move(if_block), std::move(else_statement));
}

std::unique_ptr<ast_node> else_stmt() {
  switch (cur_tok.type) {
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
  return else_block;
}

std::unique_ptr<ast_node> return_stmt() {
  match(RETURN);
  return return_stmt_B();
}

std::unique_ptr<ast_node> return_stmt_B() {
  switch (cur_tok.type) {
    case (NOT):
    case (LPAR):
    case (MINUS):
    case (BOOL_LIT):
    case (FLOAT_LIT):
    case (IDENT):
    case (INT_LIT):
      auto expression = expr();
      match(SC);
      return expression;
  }
  match(SC);
  return nullptr;
}





//will need a lookaheads due to IDENT
std::unique_ptr<ast_node> expr() {
  //determine if it is a variable assignment or a potential variable statement or function call
  if (cur_tok.type == IDENT) {
      TOKEN old = cur_tok;
      TOKEN lookahead = get_next_token();
      if (lookahead.type == ASSIGN) {
        cur_tok = old;
        put_back_token(lookahead);

        TOKEN name = cur_tok;

        match(IDENT);
        match(ASSIGN);
        auto variable_assignment = std::make_unique<var_assign_ast>(name, expr());

        return std::move(variable_assignment);
      }
  }
  return or_val();
}

std::unique_ptr<ast_node> or_val() {
  return and_val() && or_val_prime(); 
}

std::unique_ptr<ast_node> or_val_prime() {
  switch (cur_tok.type) {
  case(RPAR):
  case(SC):
  case(COMMA):
    return true;
  case (OR):
    get_next_token();
    return and_val() && or_val_prime();
  }
}

std::unique_ptr<ast_node> and_val() { return eq_val() && and_val_prime(); }

std::unique_ptr<ast_node> and_val_prime() {
  switch (cur_tok.type) {
  case(RPAR):
  case(SC):
  case(OR):
  case(COMMA):
    return true;
  case (AND):
    get_next_token();
    return eq_val() && and_val();
  }
}

std::unique_ptr<ast_node> eq_val() { return comp_val() && eq_val_prime(); }

std::unique_ptr<ast_node> eq_val_prime() {
  switch (cur_tok.type) {
  case(AND):
  case(RPAR):
  case(SC):
  case(OR):
  case(COMMA):
    return true;
  case (EQ):
  case (NE):
    get_next_token();
    return comp_val() && eq_val();
  }
}

std::unique_ptr<ast_node> comp_val() { return add_val() && comp_val_prime(); }

std::unique_ptr<ast_node> comp_val_prime() {
  switch (cur_tok.type) {
  case(NE):
  case(AND):
  case(RPAR):
  case(SC):
  case(EQ):
  case(OR):
  case(COMMA):
    return true;
  case (LE):
  case (LT):
  case (GE):
  case (GT):
    get_next_token();
    return add_val() && comp_val();
  }
}

std::unique_ptr<ast_node> add_val() {
  return mul_val() && add_val_prime();
}

std::unique_ptr<ast_node> add_val_prime() {
  switch (cur_tok.type) {
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
    TOKEN op = cur_tok;
    auto LHS = mul_val();
    auto RHS = add_val();
    auto binary_expression = std::make_unique<binary_expr_ast>(LHS, RHS);

    get_next_token();
    return std::move(binary_expression);
  }
}

std::unique_ptr<ast_node> mul_val() {

  return unary() && mul_val_prime(); 
}

std::unique_ptr<ast_node> mul_val_prime() {
  switch (cur_tok.type) {
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
      get_next_token();
      return unary() && mul_val();
  }
}

std::unique_ptr<ast_node> unary() {
  switch (cur_tok.type) {
  case (LPAR):
  case (BOOL_LIT):
  case (FLOAT_LIT):
  case (IDENT):
  case (INT_LIT):
    get_next_token();
    return identifiers();
  case (NOT):
  case (MINUS):
    TOKEN op = cur_tok;
    get_next_token();
    auto unary_expr = std::make_unique<unary_expr_ast>(op, unary());
    
    return unary_expr;
  }
}

std::unique_ptr<ast_node> identifiers() {
  switch (cur_tok.type) {
    case (INT_TOK):
      auto result = std::make_unique<int_ast_node>(cur_tok, cur_tok.lexeme);
      get_next_token();
      return std::move(result);
    case (FLOAT_TOK):
      auto result = std::make_unique<float_ast_node>(cur_tok, cur_tok.lexeme);
      get_next_token();
      return std::move(result);
    case (BOOL_TOK):
      auto result = std::make_unique<bool_ast_node>(cur_tok, cur_tok.lexeme);
      get_next_token();
      return std::move(result);
    case (IDENT):
      return identifiers_B();
    default:
      std::cerr << "Expected an identifier" << std::endl;
      exit(0);
  }
}

std::unique_ptr<ast_node> identifiers_B() {
  switch (cur_tok.type) {
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
    case(MOD):
      return nullptr;
  }
  // function call
  TOKEN callee = cur_tok;
  match(LPAR);
  std::vector<std::unique_ptr<ast_node>> arguments = args();
  
  match(RPAR);
  auto function_call = std::make_unique<call_expr_ast>(callee, arguments);
  return std::move(function_call);
}

std::vector<std::unique_ptr<ast_node>> args() {
  // there are no arguments 
  if (cur_tok.type == RPAR) {
    return nullptr;
  }

  return arg_list();
}

std::vector<std::unique_ptr<ast_node>> arg_list() {
  auto expression = expr();
  auto arg_list = arg_list_prime();

  if (arg_list == NULL) {
    return expression;
  }
}

std::unique_ptr<ast_node> arg_list_prime() {
  if (cur_tok.type == RPAR) {
    return nullptr
  }
  match(COMMA);
  expr();
  arg_list_prime();
}




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


/// void_ast_node - Class for boolean literals like true, false
class void_ast_node : public ast_node {
  TOKEN Tok;
  std::string Name;

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
    call_expr_ast(const std::string &callee,
        std::vector<std::unique_ptr<ast_node>> args) :
    Callee(callee), Args(std::move(Args)) {}
};


//class for function signature
class prototype_ast {
    TOKEN Name;
    std::vector<std::string> Args;

public:
    prototype_ast(TOKEN name, std::vector<std::string> Args) 
    : Name(name), Args(std::move(Args)) {}
};


//class for function signature and body
class function_ast {
    std::unique_ptr<prototype_ast> Proto;
    std::unique_ptr<ast_node> Body;

public:
    function_ast(std::unique_ptr<prototype_ast> proto, 
        std::unique_ptr<ast_node> body) : Proto(std::move(proto)), Body(std::move(body)) {}

};




// class for if statement structure
class if_ast {
    std::unique_ptr<ast_node> Condition;
    std::unique_ptr<ast_node> If_body;
    std::unique_ptr<ast_node> Else_body;
    
public:
  if_ast(std::unique_ptr<ast_node> condition, std::unique_ptr<ast_node> if_body, std::unique_ptr<ast_node> else_body) :
    Condition(std::move(condition)), If_body(std::move(if_body)), Else_body(std::move(else_body)) {}
};

// class for while statement structure
class while_ast {
    std::unique_ptr<ast_node> Condition;
    std::unique_ptr<ast_node> Body;
    
public:
  while_ast(std::unique_ptr<ast_node> condition, std::unique_ptr<ast_node> body) :
    Condition(std::move(condition)), Body(std::move(body)) {}
};

//class for return statement structure
class return_ast {
  std::unique_ptr<ast_node> Body;

public:
  return_ast(std::unique_ptr<ast_node> body) : Body(std::move(body)) {}
};

//class for variable declaration structure
class var_decl_ast : public ast_node {
  TOKEN Token;

public:
  var_decl_ast(TOKEN token) : Token(token)  {}
};

// class for variable assignment structure
class var_assign_ast {
  TOKEN Name;
  std::unique_ptr<ast_node> Expr;

public:
  var_assign_ast(TOKEN name, std::unique_ptr<ast_node> expr) : Name(name), Expr(std::move(expr)) {

  }
};




//===----------------------------------------------------------------------===//
// Code Generation
//===----------------------------------------------------------------------===//

static LLVMContext TheContext;
static IRBuilder<> Builder(TheContext);
static std::unique_ptr<Module> TheModule;

//===----------------------------------------------------------------------===//
// AST Printer
//===----------------------------------------------------------------------===//

inline llvm::raw_ostream &operator<<(llvm::raw_ostream &os,
                                     const ast_node &ast) {
  os << ast.to_string();
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
  get_next_token();
  while (cur_tok.type != EOF_TOK) {
    fprintf(stderr, "Token: %s with type %d\n", cur_tok.lexeme.c_str(),
            cur_tok.type);
    get_next_token();
  }
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
