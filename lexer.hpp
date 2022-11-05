#ifndef LEXER_HPP
#define LEXER_HPP

#include "common.hpp"


extern std::string IdentifierStr; // Filled in if IDENT
extern int IntVal;                // Filled in if INT_LIT
extern bool BoolVal;              // Filled in if BOOL_LIT
extern float FloatVal;            // Filled in if FLOAT_LIT
extern std::string StringVal;     // Filled in if String Literal
extern int lineNo, columnNo;

extern TOKEN CurTok;
extern std::deque<TOKEN> tok_buffer;
extern FILE *pFile;

TOKEN gettok();
TOKEN getNextToken();
void putBackToken(TOKEN tok);
void lexer();

#endif