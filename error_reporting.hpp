#ifndef ERROR_REPORTING_HPP
#define ERROR_REPORTING_HPP

#include "common.hpp"



std::string LogSyntaxError(TOKEN tok, std::string symbol);

std::string LogSemanticError();

void PrintError();



#endif