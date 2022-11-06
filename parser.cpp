#include "common.hpp"

//===----------------------------------------------------------------------===//
// Parser
//===----------------------------------------------------------------------===//

/// cur_tok/get_next_token - Provide a simple token buffer.  cur_tok is the current
/// token the parser is looking at.  get_next_token reads another token from the
/// lexer and updates cur_tok with its results.


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