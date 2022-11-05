#ifndef RECURSIVE_DESCENT_HPP
#define RECURSIVE_DESCENT_HPP

#include "common.hpp"

std::unique_ptr<ast_node> parser();
std::unique_ptr<ast_node> program();
void extern_list(std::vector<std::unique_ptr<ast_node>> list);
void extern_list_prime(std::vector<std::unique_ptr<ast_node>> list);
std::unique_ptr<ast_node> extern_();
void decl_list(std::vector<std::unique_ptr<ast_node>> list);
void decl_list_prime(std::vector<std::unique_ptr<ast_node>> list);
std::unique_ptr<ast_node> decl();
std::unique_ptr<var_decl_ast> var_decl();
std::unique_ptr<function_ast> fun_decl();
TOKEN var_type();
TOKEN type_spec();
std::vector<std::unique_ptr<ast_node>> params();
void param_list(std::vector<std::unique_ptr<ast_node>> list);
std::unique_ptr<ast_node> param();
std::unique_ptr<ast_node> block();
void local_decls(std::vector<std::unique_ptr<ast_node>> list);
void local_decls_prime(std::vector<std::unique_ptr<ast_node>> list);
std::unique_ptr<ast_node> local_decl();
void stmt_list(std::vector<std::unique_ptr<ast_node>> list);
void stmt_list_prime(std::vector<std::unique_ptr<ast_node>> list);
std::unique_ptr<ast_node> stmt();
std::unique_ptr<ast_node> expr_stmt();
std::unique_ptr<ast_node> while_stmt();
std::unique_ptr<ast_node> if_stmt();
std::unique_ptr<ast_node> else_stmt();
std::unique_ptr<ast_node> return_stmt();
std::unique_ptr<ast_node> return_stmt_B();
std::unique_ptr<ast_node> expr();
std::unique_ptr<ast_node> or_val();
std::unique_ptr<ast_node> or_val_prime(std::unique_ptr<ast_node> lhs);
std::unique_ptr<ast_node> and_val();
std::unique_ptr<ast_node> and_val_prime(std::unique_ptr<ast_node> lhs);
std::unique_ptr<ast_node> eq_val();
std::unique_ptr<ast_node> eq_val_prime(std::unique_ptr<ast_node> lhs);
std::unique_ptr<ast_node> comp_val_prime(std::unique_ptr<ast_node> lhs);
std::unique_ptr<ast_node> add_val_prime(std::unique_ptr<ast_node> lhs);
std::unique_ptr<ast_node> mul_val_prime(std::unique_ptr<ast_node> lhs);
std::unique_ptr<ast_node> unary();
std::unique_ptr<ast_node> identifiers();
std::unique_ptr<ast_node> identifiers_B();
std::vector<std::unique_ptr<ast_node>> args();
void arg_list(std::vector<std::unique_ptr<ast_node>> list);
void arg_list_prime(std::vector<std::unique_ptr<ast_node>> list);


#endif