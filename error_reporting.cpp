#include "error_reporting.hpp"

std::string LogSyntaxError(TOKEN tok, std::string symbol) {    
    std::cerr << "line: " << tok.lineNo << " col: " << tok.columnNo << " error: expected " << symbol << " but got " << tok.lexeme << std::endl;
    exit(0);
}
