#include "common.hpp"


//===----------------------------------------------------------------------===//
// Lexer
//===----------------------------------------------------------------------===//




TOKEN return_tok(std::string lex_val, int tok_type) {
  TOKEN return_tok;
  return_tok.lexeme = lex_val;
  return_tok.type = tok_type;
  return_tok.line_no = line_no;
  return_tok.column_no = column_no - lex_val.length() - 1;
  return return_tok;
}

// Read file line by line -- or look for \n and if found add 1 to line number
// and reset column number to 0
/// gettok - Return the next token from standard input.
 TOKEN gettok() {

   int last_char = ' ';
   int next_char = ' ';

  // Skip any whitespace.
  while (isspace(last_char)) {
    if (last_char == '\n' || last_char == '\r') {
      line_no++;
      column_no = 1;
    }
    last_char = getc(p_file);
    column_no++;
  }

  if (isalpha(last_char) ||
      (last_char == '_')) { // identifier: [a-zA-Z_][a-zA-Z_0-9]*
    identifier_str = last_char;
    column_no++;

    while (isalnum((last_char = getc(p_file))) || (last_char == '_')) {
      identifier_str += last_char;
      column_no++;
    }

    if (identifier_str == "int")
      return return_tok("int", INT_TOK);
    if (identifier_str == "bool")
      return return_tok("bool", BOOL_TOK);
    if (identifier_str == "float")
      return return_tok("float", FLOAT_TOK);
    if (identifier_str == "void")
      return return_tok("void", VOID_TOK);
    if (identifier_str == "bool")
      return return_tok("bool", BOOL_TOK);
    if (identifier_str == "extern")
      return return_tok("extern", EXTERN);
    if (identifier_str == "if")
      return return_tok("if", IF);
    if (identifier_str == "else")
      return return_tok("else", ELSE);
    if (identifier_str == "while")
      return return_tok("while", WHILE);
    if (identifier_str == "return")
      return return_tok("return", RETURN);
    if (identifier_str == "true") {
      bool_val = true;
      return return_tok("true", BOOL_LIT);
    }
    if (identifier_str == "false") {
      bool_val = false;
      return return_tok("false", BOOL_LIT);
    }

    return return_tok(identifier_str.c_str(), IDENT);
  }

  if (last_char == '=') {
    next_char = getc(p_file);
    if (next_char == '=') { // EQ: ==
      last_char = getc(p_file);
      column_no += 2;
      return return_tok("==", EQ);
    } else {
      last_char = next_char;
      column_no++;
      return return_tok("=", ASSIGN);
    }
  }

  if (last_char == '{') {
    last_char = getc(p_file);
    column_no++;
    return return_tok("{", LBRA);
  }
  if (last_char == '}') {
    last_char = getc(p_file);
    column_no++;
    return return_tok("}", RBRA);
  }
  if (last_char == '(') {
    last_char = getc(p_file);
    column_no++;
    return return_tok("(", LPAR);
  }
  if (last_char == ')') {
    last_char = getc(p_file);
    column_no++;
    return return_tok(")", RPAR);
  }
  if (last_char == ';') {
    last_char = getc(p_file);
    column_no++;
    return return_tok(";", SC);
  }
  if (last_char == ',') {
    last_char = getc(p_file);
    column_no++;
    return return_tok(",", COMMA);
  }

  if (isdigit(last_char) || last_char == '.') { // Number: [0-9]+.
    std::string num_str;

    if (last_char == '.') { // Floatingpoint Number: .[0-9]+
      do {
        num_str += last_char;
        last_char = getc(p_file);
        column_no++;
      } while (isdigit(last_char));

      float_val = strtof(num_str.c_str(), nullptr);
      return return_tok(num_str, FLOAT_LIT);
    } else {
      do { // Start of Number: [0-9]+
        num_str += last_char;
        last_char = getc(p_file);
        column_no++;
      } while (isdigit(last_char));

      if (last_char == '.') { // Floatingpoint Number: [0-9]+.[0-9]+)
        do {
          num_str += last_char;
          last_char = getc(p_file);
          column_no++;
        } while (isdigit(last_char));

        float_val = strtof(num_str.c_str(), nullptr);
        return return_tok(num_str, FLOAT_LIT);
      } else { // Integer : [0-9]+
        int_val = strtod(num_str.c_str(), nullptr);
        return return_tok(num_str, INT_LIT);
      }
    }
  }

  if (last_char == '&') {
    next_char = getc(p_file);
    if (next_char == '&') { // AND: &&
      last_char = getc(p_file);
      column_no += 2;
      return return_tok("&&", AND);
    } else {
      last_char = next_char;
      column_no++;
      return return_tok("&", int('&'));
    }
  }

  if (last_char == '|') {
    next_char = getc(p_file);
    if (next_char == '|') { // OR: ||
      last_char = getc(p_file);
      column_no += 2;
      return return_tok("||", OR);
    } else {
      last_char = next_char;
      column_no++;
      return return_tok("|", int('|'));
    }
  }

  if (last_char == '!') {
    next_char = getc(p_file);
    if (next_char == '=') { // NE: !=
      last_char = getc(p_file);
      column_no += 2;
      return return_tok("!=", NE);
    } else {
      last_char = next_char;
      column_no++;
      return return_tok("!", NOT);
      ;
    }
  }

  if (last_char == '<') {
    next_char = getc(p_file);
    if (next_char == '=') { // LE: <=
      last_char = getc(p_file);
      column_no += 2;
      return return_tok("<=", LE);
    } else {
      last_char = next_char;
      column_no++;
      return return_tok("<", LT);
    }
  }

  if (last_char == '>') {
    next_char = getc(p_file);
    if (next_char == '=') { // GE: >=
      last_char = getc(p_file);
      column_no += 2;
      return return_tok(">=", GE);
    } else {
      last_char = next_char;
      column_no++;
      return return_tok(">", GT);
    }
  }

  if (last_char == '/') { // could be division or could be the start of a comment
    last_char = getc(p_file);
    column_no++;
    if (last_char == '/') { // definitely a comment
      do {
        last_char = getc(p_file);
        column_no++;
      } while (last_char != EOF && last_char != '\n' && last_char != '\r');

      if (last_char != EOF)
        return gettok();
    } else
      return return_tok("/", DIV);
  }

  // Check for end of file.  Don't eat the EOF.
  if (last_char == EOF) {
    column_no++;
    return return_tok("0", EOF_TOK);
  }

  // Otherwise, just return the character as its ascii value.
  int this_char = last_char;
  std::string s(1, this_char);
  last_char = getc(p_file);
  column_no++;
  return return_tok(s, int(this_char));
}
