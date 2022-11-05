#ifndef PARSER_HPP
#define PARSER_HPP


#include "common.hpp"

extern TOKEN cur_tok;
extern std::deque<TOKEN> tok_buffer;


TOKEN get_next_token();

void match(int word);

void put_back_token(TOKEN tok);

