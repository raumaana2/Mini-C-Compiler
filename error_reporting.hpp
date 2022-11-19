#ifndef ERROR_REPORTING_HPP
#define ERROR_REPORTING_HPP

#include "common.hpp"


void LinePrinter(TOKEN tok);

void LogSymbolError(TOKEN tok, std::string symbol);

void LogSemanticError();

void LogSyntaxError(TOKEN tok, std::string message);




#endif