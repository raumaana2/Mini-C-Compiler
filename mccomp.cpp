
#include "common.hpp"



//===----------------------------------------------------------------------===//
// Main driver code.
//===----------------------------------------------------------------------===//

int main(int argc, char **argv) {
  if (argc == 2) {
    p_file = fopen(argv[1], "r");
    if (p_file == NULL)
      perror("Error opening file");
  } else {
    std::cout << "Usage: ./code InputFile\n";
    return 1;
  }

  // initialize line number and column numbers to zero
  line_no = 1;
  column_no = 1;

  // get the first token
  get_next_token();
  while (cur_tok.type != EOF_TOK) {
    fprintf(stderr, "Token: %s with type %d\n", cur_tok.lexeme.c_str(),
            cur_tok.type);
    get_next_token();
  }
  fprintf(stderr, "Lexer Finished\n");

  // Make the module, which holds all the code.
  the_module = std::make_unique<Module>("mini-c", the_context);

  // Run the parser now.
  parser();
  fprintf(stderr, "Parsing Finished\n");

  //********************* Start printing final IR **************************
  // Print out all of the generated code into a file called output.ll
  auto file_name = "output.ll";
  std::error_code EC;
  raw_fd_ostream dest(file_name, EC, sys::fs::OF_None);

  if (EC) {
    errs() << "Could not open file: " << EC.message();
    return 1;
  }
  // the_module->print(errs(), nullptr); // print IR to terminal
  the_module->print(dest, nullptr);
  //********************* End printing final IR ****************************

  fclose(p_file); // close the file that contains the code that was parsed
  return 0;
}
