#include "error_reporting.hpp"

void LogSymbolError(TOKEN tok, std::string symbol) {    
    std::cerr << fileName <<  ":" << tok.lineNo << ":" << tok.columnNo << " \033[31merror\033[0m: expected " << symbol << " but got \"" << tok.lexeme << "\""<< std::endl;
    LinePrinter(tok);
}


void LogSyntaxError(TOKEN tok, std::string message) {
    std::cerr << fileName <<  ":" << tok.lineNo << ":" << tok.columnNo << " \033[31merror\033[0m: " << message << " but got \"" << tok.lexeme << "\""<< std::endl;
    LinePrinter(tok);
}



void LinePrinter(TOKEN tok) {
    std::ifstream input(fileName);
    std::string line;
    int col_no = tok.columnNo;
    int i = 0;
    while (i != tok.lineNo && std::getline(input, line)) {
        ++i;
    }
    int tab_found = 0;
    //consider the tab character which is treated as one character in the lexer but multiple characters
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
    exit(0);
}

void LogWarnings() {
    while (!WarningStack.empty()) {
        std::cout << WarningStack.top() << std::endl;
        WarningStack.pop();
    }
}