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

/* add other AST nodes as nessasary */


//===----------------------------------------------------------------------===//
// Parser
//===----------------------------------------------------------------------===//

/// CurTok/getNextToken - Provide a simple token buffer.  CurTok is the current
/// token the parser is looking at.  getNextToken reads another token from the
/// lexer and updates CurTok with its results.
static TOKEN CurTok;
static std::deque<TOKEN> tok_buffer;
static TOKEN lookahead[3];

static TOKEN getNextToken() {

  if (tok_buffer.size() == 0)
    tok_buffer.push_back(gettok());

  TOKEN temp = tok_buffer.front();
  tok_buffer.pop_front();

  return CurTok = temp;
}

static bool match(int word) {
  if (CurTok.type == word) {
    getNextToken();
    return true;
  }
  return false;
}

static void putBackToken(TOKEN tok) { tok_buffer.push_front(tok); }


//===----------------------------------------------------------------------===//
// Recursive Descent Parser - Function call for each production
//===----------------------------------------------------------------------===//

/* Add function calls for each production */

// program ::= extern_list decl_list
static bool parser() {
  // add body

  if (program()) {
    switch (CurTok.type) {
    case (EOF_TOK):
      return true;
    default:
      return false;
    }
  }
}

static bool program() {
  switch (CurTok.type) {
    case (EXTERN):
      return extern_list() && decl_list();
    case (BOOL_TOK):
    case (FLOAT_TOK):
    case (INT_TOK):
    case (VOID_TOK):
      return decl_list();
  }
  return false;
}

static bool extern_list() {
  return extern_() && extern_list_prime(); 
}



static bool extern_list_prime() {
  switch (CurTok.type) {
    case (BOOL_TOK):
    case (FLOAT_TOK):
    case (INT_TOK):
    case (VOID_TOK):
      return true;
  }
  return extern_() && extern_list_prime();
}

static bool extern_() {
  return match(EXTERN) && type_spec() && match(IDENT) && match(LPAR) &&
         params() && match(RPAR) && match(SC);
}

static bool decl_list() {
  return decl() && decl_list_prime(); 
}

static bool decl_list_prime() {
  switch (CurTok.type) {
    case (EOF_TOK):
      return true;
  }
  return decl() && decl_list_prime(); 
}

/////////////////

static bool decl() { return var_decl() || fun_decl(); }

static bool var_decl() { return var_type() && match(IDENT) && match(SC); }

static bool fun_decl() {
  return type_spec() && match(IDENT) && match(LPAR) && params() &&
         match(RPAR) && block();
}

static bool var_type() {
  return match(INT_TOK) || match(FLOAT_TOK) || match(BOOL_TOK);
}

static bool type_spec() { return match(VOID_TOK) || var_type(); }

/////////////////

static bool params() {
  switch (CurTok.type) {
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

static bool param_list() {
  return param() && param_list_prime();
}

static bool param_list_prime() {
  switch (CurTok.type) {
  case (RPAR):
    return true;
  case (COMMA):
    return match(COMMA) && param() && param_list_prime();
  }
  return false;
}

static bool param() {
  return var_type() && match(IDENT); 
}

static bool block() {
  return match(LBRA) && local_decls() && stmt_list() && match(RBRA);
}

static bool local_decls() {
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
      return true;
  }
  return local_decls_prime(); 
}

static bool local_decls_prime() {
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
      return true;
  }
  return local_decl() && local_decls_prime();
}

static bool local_decl() { 
  return var_type() && match(IDENT) && match(SC);
}

static bool stmt_list() { 
  if (CurTok.type == RBRA) {
    return true;
  }
  return stmt_list_prime();
}

static bool stmt_list_prime() {
  if (CurTok.type == RBRA) {
    return true;
  }
  return stmt() && stmt_list_prime();
}

static bool stmt() {
  switch (CurTok.type) {
  case (NOT):
  case (LPAR):
  case (MINUS):
  case (SC):
  case (BOOL_LIT):
  case (FLOAT_LIT):
  case (IDENT):
  case (INT_LIT):
    getNextToken();
    return expr_stmt();
  case (LBRA):
    getNextToken();
    return block();
  case (IF):
    getNextToken();
    return if_stmt();
  case (WHILE):
    getNextToken();
    return while_stmt();
  case (RETURN):
    getNextToken();
    return return_stmt();
  }
  return false;
}

static bool expr_stmt() {
  switch (CurTok.type) {
    case (NOT):
    case (LPAR):
    case (MINUS):
    case (SC):
    case (BOOL_LIT):
    case (FLOAT_LIT):
    case (IDENT):
    case (INT_LIT):
      return expr() && match(SC);
  }
  return match(SC); 
}

static bool while_stmt() {
  return match(WHILE) && match(LPAR) && expr() && match(RPAR) && stmt();
}

static bool if_stmt() {
  return match(IF) && match(LPAR) && expr() && match(RPAR) && block() &&
         else_stmt();
}

static bool else_stmt() {
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
      return true;
  }
  return match(ELSE) && block(); 
}

static bool return_stmt() {
  return match(RETURN) && return_stmt_B(); }

static bool return_stmt_B() {
  switch (CurTok.type) {
    case (NOT):
    case (LPAR):
    case (MINUS):
    case (BOOL_LIT):
    case (FLOAT_LIT):
    case (IDENT):
    case (INT_LIT):
      return expr() && match(SC);
  }
  return match(SC);
}

//will need 2 lookaheads due to IDENT
static bool expr() {

}

static bool or_val() { return and_val() && or_val_prime(); }

static bool or_val_prime() {
  switch (CurTok.type) {
  case(RPAR):
  case(SC):
  case(COMMA):
    return true;
  case (OR):
    getNextToken();
    return and_val() && or_val();
  default:
    return true;
  }
}

static bool and_val() { return eq_val() && and_val_prime(); }

static bool and_val_prime() {
  switch (CurTok.type) {
  case(RPAR):
  case(SC):
  case(OR):
  case(COMMA):
    return true;
  case (AND):
    getNextToken();
    return eq_val() && and_val();
  default:
    return true;
  }
}

static bool eq_val() { return comp_val() && eq_val_prime(); }

static bool eq_val_prime() {
  switch (CurTok.type) {
  case(AND):
  case(RPAR):
  case(SC):
  case(OR):
  case(COMMA):
    return true;
  case (EQ):
  case (NE):
    getNextToken();
    return comp_val() && eq_val();
  default:
    return true;
  }
}

static bool comp_val() { return add_val() && comp_val_prime(); }

static bool comp_val_prime() {
  switch (CurTok.type) {
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
    getNextToken();
    return add_val() && comp_val();
  default:
    return true;
  }
}

static bool add_val() { return mul_val() && add_val_prime(); }

static bool add_val_prime() {
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
    return true;
  case (PLUS):
  case (MINUS):
    getNextToken();
    return mul_val() && add_val();
  default:
    return true;
  }
}

static bool mul_val() { return unary() && mul_val_prime(); }

static bool mul_val_prime() {
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
      return true;
    case (ASTERIX):
    case (DIV):
    case (MOD):
      getNextToken();
      return unary() && mul_val();
    default:
      return true;
  }
}

static bool unary() {
  switch (CurTok.type) {
  case (LPAR):
  case (BOOL_LIT):
  case (FLOAT_LIT):
  case (IDENT):
  case (INT_LIT):
    getNextToken();
    return identifiers();
  case (NOT):
  case (MINUS):
    getNextToken();
    return unary();
  }
}

static bool identifiers() {
  switch (CurTok.type) {
  case (INT_TOK):
  case (FLOAT_TOK):
  case (BOOL_TOK):
    getNextToken();
    return true;
  case (IDENT):
    return identifiers_B();
  default:
    return false;
  }
}

static bool identifiers_B() {
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
    case(MOD):
      return true;
  }
  return match(LPAR) && args() && match(RPAR);
}

static bool args() {
  if (CurTok.type == RPAR) {
    return true;
  }
  return arg_list();
}

static bool arg_list() {
  return expr() && arg_list_prime(); 
}

static bool arg_list_prime() {
  if (CurTok.type == RPAR) {
    return true;
  }
  return match(COMMA) && expr() && arg_list_prime();
}