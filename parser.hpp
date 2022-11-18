#ifndef PARSER_HPP
#define PARSER_HPP

#include "common.hpp"
#include "lexer.hpp"
#include "ast_node.hpp"
#include "error_reporting.hpp"

TOKEN getNextToken();
void putBackToken(TOKEN tok);
void match(int word);

std::unique_ptr<ASTNode> parser();
std::unique_ptr<ASTNode> program();
void extern_list(std::vector<std::unique_ptr<ASTNode>> &list);
void extern_list_prime(std::vector<std::unique_ptr<ASTNode>> &list);
std::unique_ptr<ASTNode> extern_();
void decl_list(std::vector<std::unique_ptr<ASTNode>> &list);
void decl_list_prime(std::vector<std::unique_ptr<ASTNode>> &list);
std::unique_ptr<ASTNode> decl();
std::unique_ptr<VarDeclAST> var_decl();
std::unique_ptr<FunctionAST> fun_decl();
TOKEN var_type();
TOKEN type_spec();
std::vector<std::unique_ptr<VarDeclAST>> params();
void param_list(std::vector<std::unique_ptr<VarDeclAST>> &list);
void param_list_prime(std::vector<std::unique_ptr<VarDeclAST>> &list);
std::unique_ptr<VarDeclAST> param();
std::unique_ptr<BlockAST> block();
void local_decls(std::vector<std::unique_ptr<ASTNode>> &list);
void local_decls_prime(std::vector<std::unique_ptr<ASTNode>> &list);
std::unique_ptr<ASTNode> local_decl();
void stmt_list(std::vector<std::unique_ptr<ASTNode>> &list);
void stmt_list_prime(std::vector<std::unique_ptr<ASTNode>> &list);
std::unique_ptr<ASTNode> stmt();
std::unique_ptr<ASTNode> expr_stmt();
std::unique_ptr<ASTNode> while_stmt();
std::unique_ptr<ASTNode> if_stmt();
std::unique_ptr<ASTNode> else_stmt();
std::unique_ptr<ASTNode> return_stmt();
std::unique_ptr<ASTNode> return_stmt_B();
std::unique_ptr<ASTNode> expr();
std::unique_ptr<ASTNode> or_val();
std::unique_ptr<ASTNode> or_val_prime(std::unique_ptr<ASTNode> lhs);
std::unique_ptr<ASTNode> and_val();
std::unique_ptr<ASTNode> and_val_prime(std::unique_ptr<ASTNode> lhs);
std::unique_ptr<ASTNode> eq_val();
std::unique_ptr<ASTNode> eq_val_prime(std::unique_ptr<ASTNode> lhs);
std::unique_ptr<ASTNode> comp_val();
std::unique_ptr<ASTNode> comp_val_prime(std::unique_ptr<ASTNode> lhs);
std::unique_ptr<ASTNode> add_val();
std::unique_ptr<ASTNode> add_val_prime(std::unique_ptr<ASTNode> lhs);
std::unique_ptr<ASTNode> mul_val();
std::unique_ptr<ASTNode> mul_val_prime(std::unique_ptr<ASTNode> lhs);
std::unique_ptr<ASTNode> unary();
std::unique_ptr<ASTNode> identifiers();
std::unique_ptr<ASTNode> identifiers_B();
std::vector<std::unique_ptr<ASTNode>> args();
void arg_list(std::vector<std::unique_ptr<ASTNode>> &list);
void arg_list_prime(std::vector<std::unique_ptr<ASTNode>> &list);




#endif 