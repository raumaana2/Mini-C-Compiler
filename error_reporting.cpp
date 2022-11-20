#include "error_reporting.hpp"


/**
 * Print symbol syntax errors found in parser
 * @param  {TOKEN} tok          : token that stores symbol, line number, column number
 * @param  {std::string} symbol : symbol found
 */
void LogSymbolError(TOKEN tok, std::string symbol) {    
    std::cerr << fileName <<  ":" << tok.lineNo << ":" << tok.columnNo << " \033[31merror\033[0m: expected " << symbol << " but got \"" << tok.lexeme << "\""<< std::endl;
    LinePrinter(tok, true); //print line of code from file
}

/**
 * Print syntax errors found in parser, used for more descriptive errors
 * @param  {TOKEN} tok           : token that stores symbol, line number, column number
 * @param  {std::string} message : message that describes error
 */
void LogSyntaxError(TOKEN tok, std::string message) {
    std::cerr << fileName <<  ":" << tok.lineNo << ":" << tok.columnNo << " \033[31merror\033[0m: " << message << " but got \"" << tok.lexeme << "\""<< std::endl;
    LinePrinter(tok, true); //print line of code from file
}

/**
 * Print semantic errors found during codegen
 * @param  {TOKEN} tok           : token that stores symbol, line number, column number
 * @param  {std::string} message : message that describes error
 */
void LogSemanticError(TOKEN tok, std::string message) {
    std::cerr << fileName <<  ":" << tok.lineNo << ":" << tok.columnNo << " \033[31merror\033[0m: " << message << std::endl;
    LinePrinter(tok, true); //print line of code from file
}

/**
 * Print warnings found during codegen
 */
void LogWarnings() {
    while (!WarningQueue.empty()) {
        std::tuple<TOKEN, std::string> warning = WarningQueue.front();
        std::cerr << fileName <<  ":" << std::get<0>(warning).lineNo << ":" << std::get<0>(warning).columnNo << " \033[35mwarning\033[0m: " << std::get<1>(warning) << std::endl;
        LinePrinter(std::get<0>(warning), false); //print line of code from file
        WarningQueue.pop();
    }
}

void LinePrinter(TOKEN tok, bool exit_flag) {
    std::ifstream input(fileName);
    std::string line;
    int col_no = tok.columnNo;
    int i = 0;
    while (i != tok.lineNo && std::getline(input, line)) {
        ++i;
    }
    int tab_found = 0;
    //consider the tab character which is treated as one character in the lexer
    //but treated as multiple characters by this function  
    if (line.find("\t") != std::string::npos) {
        tab_found = 1;
        col_no--;
    } 
    if (col_no < 0) 
        col_no = 0;
    std::cerr << line << std::endl;
    std::string whitespace(col_no - 1, ' ');
    std::string tab(tab_found, '\t');
    std::cerr << tab << whitespace << "\033[32m^\033[0m"<< std::endl;
    input.close();
    if (exit_flag) //exit if specified, usually exit on errors but dont exit on warnings
        exit(0);
}


