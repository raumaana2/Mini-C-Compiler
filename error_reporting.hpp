#ifndef ERROR_REPORTING_HPP
#define ERROR_REPORTING_HPP

#include "common.hpp"

//queue for warnings found during codegen and stores a tuple of token and warning message
extern std::queue<std::tuple<TOKEN, std::string>> WarningQueue; 


void LinePrinter(TOKEN tok, bool exit_flag);

void LogSymbolError(TOKEN tok, std::string symbol);

void LogSemanticError(TOKEN tok, std::string message);

void LogSyntaxError(TOKEN tok, std::string message);

void LogWarnings();




#endif