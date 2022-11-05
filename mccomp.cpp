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

void put_back_token(TOKEN tok) {
  tok_buffer.push_front(tok); 
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
class literal_ast_node : public ast_node {
  TOKEN Tok;
  

public:
  literal_ast_node(TOKEN tok) : Tok(tok) {}
  virtual Value *codegen() override;
  // virtual std::string to_string() const override {
  // return a sting representation of this AST node
  //};
};


// /// int_ast_node - Class for integer literals like 1, 2, 10,
// class int_ast_node : public ast_node {
//   int Val;
//   TOKEN Tok;
//   std::string Name;

// public:
//   int_ast_node(TOKEN tok, int val) : Val(val), Tok(tok) {}
//   virtual Value *codegen() override;
//   // virtual std::string to_string() const override {
//   // return a sting representation of this AST node
//   //};
// };

// /// float_ast_node - Class for float literals like 1.0, 2.5, 10.000001,
// class float_ast_node : public ast_node {
//   float Val;
//   TOKEN Tok;
//   std::string Name;

// public:
//   float_ast_node(TOKEN tok, float val) : Val(val), Tok(tok) {}
//   virtual Value *codegen() override;
//   // virtual std::string to_string() const override {
//   // return a sting representation of this AST node
//   //};
// };


// /// bool_ast_node - Class for boolean literals like true, false
// class bool_ast_node : public ast_node {
//   bool Val;
//   TOKEN Tok;
//   std::string Name;

// public:
//   bool_ast_node(TOKEN tok, bool val) : Val(val), Tok(tok) {}
//   virtual Value *codegen() override;
//   // virtual std::string to_string() const override {
//   // return a sting representation of this AST node
//   //};
// };


/// void_ast_node - Class for boolean literals like true, false
class void_ast_node : public ast_node {
  TOKEN Tok;

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
    call_expr_ast(TOKEN callee,
        std::vector<std::unique_ptr<ast_node>> args) :
    Callee(callee), Args(std::move(Args)) {}
};


//class for function signature
class prototype_ast : public ast_node {
    TOKEN Type;
    TOKEN Name;
    std::vector<std::unique_ptr<ast_node>> Args;

public:
    prototype_ast(TOKEN type, TOKEN name, std::vector<std::unique_ptr<ast_node>> args) 
    : Type(type), Name(name), Args(std::move(args)) {}
};


//class for function signature and body
class function_ast : public ast_node  {
    std::unique_ptr<prototype_ast> Proto;
    std::unique_ptr<ast_node> Body;

public:
    function_ast(std::unique_ptr<prototype_ast> proto, 
        std::unique_ptr<ast_node> body) : Proto(std::move(proto)), Body(std::move(body)) {}

};


// class for if statement structure
class if_ast : public ast_node {
    std::unique_ptr<ast_node> Condition;
    std::unique_ptr<ast_node> If_body;
    std::unique_ptr<ast_node> Else_body;
    
public:
  if_ast(std::unique_ptr<ast_node> condition, std::unique_ptr<ast_node> if_body, std::unique_ptr<ast_node> else_body) :
    Condition(std::move(condition)), If_body(std::move(if_body)), Else_body(std::move(else_body)) {}
};

// class for while statement structure
class while_ast : public ast_node {
    std::unique_ptr<ast_node> Condition;
    std::unique_ptr<ast_node> Body;
    
public:
  while_ast(std::unique_ptr<ast_node> condition, std::unique_ptr<ast_node> body) :
    Condition(std::move(condition)), Body(std::move(body)) {}
};

//class for return statement structure
class return_ast : public ast_node {
  std::unique_ptr<ast_node> Body;

public:
  return_ast(std::unique_ptr<ast_node> body) : Body(std::move(body)) {}
};

//class for variable declaration structure
class var_decl_ast : public ast_node {
  TOKEN Type;
  TOKEN Name;

public:
  var_decl_ast(TOKEN type, TOKEN name) : Type(type), Name(name)  {}
};

// class for variable assignment structure
class var_assign_ast : public ast_node {
  TOKEN Name;
  std::unique_ptr<ast_node> Expr;

public:
  var_assign_ast(TOKEN name, std::unique_ptr<ast_node> expr) : Name(name), Expr(std::move(expr)) {

  }
};


class scope_ast : public ast_node {
  std::vector<std::unique_ptr<ast_node>> Lists;

public:
  scope_ast(std::vector<std::unique_ptr<ast_node>> lists) : Lists(std::move(lists)) {}
};

class identifier_ast : public ast_node {
  TOKEN Identifier;

public: 
  identifier_ast(TOKEN identifier) : Identifier(identifier) {}
};

//===----------------------------------------------------------------------===//
// Recursive Descent Parser - Function call for each production
//===----------------------------------------------------------------------===//

/* Add function calls for each production */

std::unique_ptr<ast_node> parser() {
  // add body
  auto program_scope = program();
  
  switch (cur_tok.type) {
    case (EOF_TOK):
      return program_scope;
    default:
      //error
      exit(0);
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

std::unique_ptr<var_decl_ast> var_decl() {
  TOKEN type = var_type();
  TOKEN name = cur_tok;
  auto variable_declaration = std::make_unique<var_decl_ast>(type, name);
  match(IDENT);
  match(SC);

  return variable_declaration;
}

std::unique_ptr<function_ast> fun_decl() {
  auto type = type_spec();
  match(IDENT);
  match(LPAR); 
  if (cur_tok.type == VOID_TOK) {

  } 
  std::vector<std::unique_ptr<ast_node>> parameters = params();

  match(RPAR);
  block();

  auto scope_block = block();
  auto prototype = std::make_unique<prototype_ast>(type, parameters);
  auto function = std::make_unique<function_ast>(std::move(prototype), block);

  return std::move(function);

}

TOKEN var_type() {    
  if (cur_tok.type == INT_TOK || FLOAT_TOK || BOOL_TOK) {
    return cur_tok;
  }   
  //error             
  exit(0);                           
}

TOKEN type_spec() { 
  if (cur_tok.type == VOID_TOK) {
    return cur_tok;
  }
  return var_type();
}


std::vector<std::unique_ptr<ast_node>> params() {
  std::vector<std::unique_ptr<ast_node>> parameter_list;
  switch (cur_tok.type) {
  case (RPAR):
    // return empty list to signify no arguments  
    return std::move(parameter_list);
  case (BOOL_TOK):
  case (FLOAT_TOK):
  case (INT_TOK):
    param_list(parameter_list);
    return std::move(parameter_list);
  case (VOID_TOK):
    TOKEN tok = cur_tok;
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
  if (cur_tok.type != RPAR) {
    auto parameter = param();
    list.push_back(std::move(parameter));
    if (cur_tok.type == COMMA) {
      param_list_prime(std::move(list));
    }
  }
}


std::unique_ptr<ast_node> param() {
  TOKEN type = var_type();
  TOKEN name = cur_tok;
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
  auto local_declarations = local_declaration_list;
  auto statements = statement_list;

  auto scope = std::make_unique<scope_ast>(local_declarations, statements);

  match(RBRA);

  return std::move(scope);
}


void local_decls(std::vector<std::unique_ptr<ast_node>> list) {
  local_decls_prime(std::move(list));
}


void local_decls_prime(std::vector<std::unique_ptr<ast_node>> list) {

  switch (cur_tok.type) {
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
  TOKEN identifier = cur_tok;
  match(IDENT);
  auto variable_declaration = std::make_unique<var_decl_ast>(type, identifier);
  match(SC);
  return std::move(variable_declaration);
}

void stmt_list(std::vector<std::unique_ptr<ast_node>> list) {
  stmt_list_prime(std::move(list));
}

void stmt_list_prime(std::vector<std::unique_ptr<ast_node>> list) {
  if (cur_tok.type != RBRA) {
    auto statement = stmt();
    list.push_back(std::move(statement));
    stmt_list(std::move(list));
  }
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
  return std::move(else_block);
}

std::unique_ptr<ast_node> return_stmt() {
  match(RETURN);
  auto return_statement = std::make_unique<return_ast>(std::move(return_stmt_B));


  return std::move(return_statement);
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
      return std::move(expression);
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
  auto lhs = and_val();
  lhs = or_val_prime(std::move(lhs)); 

  return std::move(lhs); 
}

std::unique_ptr<ast_node> or_val_prime(std::unique_ptr<ast_node> lhs) {
  TOKEN op;
  
  switch (cur_tok.type) {
  case(RPAR):
  case(SC):
  case(COMMA):
    return nullptr;
  case (OR):
    op = cur_tok;
    get_next_token();

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
  switch (cur_tok.type) {
  case(RPAR):
  case(SC):
  case(OR):
  case(COMMA):
    return nullptr;
  case (AND):
    op = cur_tok;
    get_next_token();

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
}

std::unique_ptr<ast_node> eq_val() {
  auto lhs = comp_val();
  lhs = eq_val_prime(std::move(lhs));
  return std::move(lhs); 
}

std::unique_ptr<ast_node> eq_val_prime(std::unique_ptr<ast_node> lhs) {
  TOKEN op;
  switch (cur_tok.type) {
  case(AND):
  case(RPAR):
  case(SC):
  case(OR):
  case(COMMA):
    return nullptr;
  case (EQ):
  case (NE):
    op = cur_tok;
    get_next_token();

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
}

std::unique_ptr<ast_node> comp_val() { 
  auto lhs = add_val();
  lhs = comp_val_prime(std::move(lhs));

  return std::move(lhs);
}

std::unique_ptr<ast_node> comp_val_prime(std::unique_ptr<ast_node> lhs) {
  TOKEN op;
  switch (cur_tok.type) {
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
    op = cur_tok;
    get_next_token();

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
}

std::unique_ptr<ast_node> add_val() {
  auto lhs = mul_val();
  lhs = add_val_prime(std::move(lhs));

  return std::move(lhs);
}

std::unique_ptr<ast_node> add_val_prime(std::unique_ptr<ast_node> lhs) {
  TOKEN op;
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
      op = cur_tok;
      get_next_token();

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
}

std::unique_ptr<ast_node> mul_val() {
  auto lhs = unary();
  lhs = mul_val_prime(std::move(lhs));
  return std::move(lhs);
}

std::unique_ptr<ast_node> mul_val_prime(std::unique_ptr<ast_node> lhs) {
  TOKEN op;
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
      op = cur_tok;
      get_next_token();

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
    auto unary_expression = std::make_unique<unary_expr_ast>(op, unary());
    
    return std::move(unary_expression);
  }
}

std::unique_ptr<ast_node> identifiers() {
  TOKEN tok;
  if (cur_tok.type == INT_LIT || FLOAT_LIT || BOOL_LIT) {
    tok = cur_tok;
    get_next_token();
    auto result = std::make_unique<literal_ast_node>(tok);
    return std::move(result);

  } else if (cur_tok.type == IDENT) {
    return identifiers_B();               
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
  std::vector<std::unique_ptr<ast_node>> argument_list;
  // if not the follow case, we will populate the list otherwise we simply return an empty list to show we have no args
  if (cur_tok.type != RPAR) {
    arg_list(std::move(argument_list));
  }

  return argument_list;
}

void arg_list(std::vector<std::unique_ptr<ast_node>> list) {
  auto expression = expr();
  list.push_back(std::move(expression));
  
  arg_list_prime(std::move(list));

  
}

void arg_list_prime(std::vector<std::unique_ptr<ast_node>> list) {
  if (cur_tok.type != RPAR) {
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
