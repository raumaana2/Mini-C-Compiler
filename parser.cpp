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
      return extern_list() && decl_list();
    case (BOOL_TOK):
    case (FLOAT_TOK):
    case (INT_TOK):
    case (VOID_TOK):
      return decl_list();
  }
  return false;
}

std::unique_ptr<ast_node> extern_list() {
  auto _extern_ = extern_();
  auto extern_list_prime();

}



std::unique_ptr<ast_node> extern_list_prime() {
  switch (cur_tok.type) {
    case (BOOL_TOK):
    case (FLOAT_TOK):
    case (INT_TOK):
    case (VOID_TOK):
      return nullptr;
  }
  auto extern_ = extern_();
  auto extern_list_prime = extern_list_prime();
}

std::unique_ptr<ast_node> extern_() {
  match(EXTERN);
  auto type_spec = type_spec();
  match(IDENT);
  match(LPAR);
  auto params = params();
  match(RPAR);
  match(SC);
  
}

std::unique_ptr<ast_node> decl_list() {
  auto decleration = decl();
  auto decleration_list_prime = decl_list_prime();

}

std::unique_ptr<ast_node> decl_list_prime() {
  switch (cur_tok.type) {
    case (EOF_TOK):
      return nullptr;
  }
  auto decleration = decl();
  auto decleration_list = decl_list_prime();
  
}


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
  auto variable_type = type;
  match(IDENT);
  match(SC);

  return variable_type;
}

std::unique_ptr<ast_node> fun_decl() {
  return type_spec() && match(IDENT) && match(LPAR) && params() &&
         match(RPAR) && block();
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
  if (cur_tok.type == IDENT) {
      TOKEN old = cur_tok;
      TOKEN lookahead = get_next_token();
      if (lookahead.type == ASSIGN) {
        cur_tok = old;
        put_back_token(lookahead);
        match(IDENT);
        match(ASSIGN);
        return expr();
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
    return and_val() && or_val();
  default:
    return true;
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
  default:
    return true;
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
  default:
    return true;
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
  default:
    return true;
  }
}

std::unique_ptr<ast_node> add_val() { return mul_val() && add_val_prime(); }

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
    return true;
  case (PLUS):
  case (MINUS):
    get_next_token();
    return mul_val() && add_val();
  default:
    return true;
  }
}

std::unique_ptr<ast_node> mul_val() { return unary() && mul_val_prime(); }

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
      return true;
    case (ASTERIX):
    case (DIV):
    case (MOD):
      get_next_token();
      return unary() && mul_val();
    default:
      return true;
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
    auto unary = std::make_unique<unary_expr_ast>(op, unary());
    
    return unary;
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
  // dealing with function call
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
  expr();
  arg_list_prime();
}

std::unique_ptr<ast_node> arg_list_prime() {
  if (cur_tok.type == RPAR) {
    return nullptr
  }
  match(COMMA);
  expr();
  arg_list_prime();
}
