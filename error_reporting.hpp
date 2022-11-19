#ifndef ERROR_REPORTING_HPP
#define ERROR_REPORTING_HPP

#include "common.hpp"

extern std::stack<std::tuple<TOKEN, std::string> > WarningStack;


void LinePrinter(TOKEN tok);

void LogSymbolError(TOKEN tok, std::string symbol);

void LogSemanticError(TOKEN tok, std::string message);

void LogSyntaxError(TOKEN tok, std::string message);

void LogWarnings();




#endif