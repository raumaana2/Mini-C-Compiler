#include "common.hpp"

//===----------------------------------------------------------------------===//
// Recursive Descent Parser - Function call for each production
//===----------------------------------------------------------------------===//


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
  std::vector<std::unique_ptr<ast_node>> extern_vector;
  std::vector<std::unique_ptr<ast_node>> decl_vector;

  if (cur_tok.type == EXTERN) {
    extern_list(std::move(extern_vector));
    decl_list(std::move(decl_vector));

  } else if (cur_tok.type == BOOL_TOK || FLOAT_TOK || INT_TOK || VOID_TOK)  {
    decl_list(std::move(decl_vector));
  } else {
    //error
    exit(0);
  }
  // auto extern_lis = extern_vector;
  // auto declaration_list = decl_vector;

  auto scope = std::make_unique<scope_ast>(std::move(extern_vector), std::move(decl_vector));

  return std::move(scope);


}


void extern_list(std::vector<std::unique_ptr<ast_node>> list) {
  auto _extern_ = extern_();
  list.push_back(std::move(_extern_));
  extern_list_prime(std::move(list));
}

void extern_list_prime(std::vector<std::unique_ptr<ast_node>> list) {
  if (cur_tok.type != INT_TOK || FLOAT_TOK || BOOL_TOK || VOID_TOK) {
    auto _extern_ = extern_();
    list.push_back(std::move(_extern_));
    extern_list_prime(std::move(list));
  }
}

std::unique_ptr<ast_node> extern_() {
  match(EXTERN);
  auto type = type_spec();
  TOKEN identifier = cur_tok;
  match(IDENT);
  match(LPAR);
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
  if (cur_tok.type != EOF_TOK) {
    auto declaration = decl();
    list.push_back(std::move(declaration));
    decl_list_prime(std::move(list));
  }
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
  exit(0);
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
  TOKEN identifier = cur_tok;
  match(IDENT);
  match(LPAR); 
  if (cur_tok.type == VOID_TOK) {

  } 
  std::vector<std::unique_ptr<ast_node>> parameters = params();

  match(RPAR);
  auto scope = block();

  auto scope_block = block();
  auto prototype = std::make_unique<prototype_ast>(type, identifier, std::move(parameters));
  auto function = std::make_unique<function_ast>(std::move(prototype), std::move(scope));

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
    param_list(std::move(parameter_list));
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
  auto return_body = return_stmt_B();
  auto return_statement = std::make_unique<return_ast>(std::move(return_body));


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
  exit(0);
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
  exit(0);
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
  exit(0);
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
  exit(0);
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
  exit(0);
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
  exit(0);
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
  auto function_call = std::make_unique<call_expr_ast>(callee, std::move(arguments));
  return std::move(function_call);
}

std::vector<std::unique_ptr<ast_node>> args() {
  std::vector<std::unique_ptr<ast_node>> argument_list;
  // if not the follow case, we will populate the list otherwise we simply return an empty list to show we have no args
  if (cur_tok.type != RPAR) {
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
  if (cur_tok.type != RPAR) {
    match(COMMA);
    auto expression = expr();
    list.push_back(std::move(expression));
    arg_list_prime(std::move(list));
  }
}

