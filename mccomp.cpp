#include "mccomp.hpp"
#include <typeinfo>
using namespace llvm;
using namespace llvm::sys;

FILE *pFile;

//===----------------------------------------------------------------------===//
// Lexer
//===----------------------------------------------------------------------===//

static std::string IdentifierStr; // Filled in if IDENT
static int IntVal;                // Filled in if INT_LIT
static bool BoolVal;              // Filled in if BOOL_LIT
static float FloatVal;            // Filled in if FLOAT_LIT
static std::string StringVal;     // Filled in if String Literal
static int lineNo, columnNo;

//===----------------------------------------------------------------------===//
// Code Generation
//===----------------------------------------------------------------------===//

static LLVMContext TheContext;
static IRBuilder<> Builder(TheContext);
static std::unique_ptr<Module> TheModule;



static AllocaInst* CreateEntryBlockAlloca(Function *TheFunction, const std::string &VarName) {
  IRBuilder<> TmpB(&TheFunction->getEntryBlock(), TheFunction->getEntryBlock().begin());

  return TmpB.CreateAlloca(Type::getInt32Ty(TheContext), 0, VarName.c_str());
}

static TOKEN returnTok(std::string lexVal, int tok_type) {
  TOKEN return_tok;
  return_tok.lexeme = lexVal;
  return_tok.type = tok_type;
  return_tok.lineNo = lineNo;
  return_tok.columnNo = columnNo - lexVal.length() - 1;
  return return_tok;
}

// Read file line by line -- or look for \n and if found add 1 to line number
// and reset column number to 0
/// gettok - Return the next token from standard input.
static TOKEN gettok() {

  static int LastChar = ' ';
  static int NextChar = ' ';

  // Skip any whitespace.
  while (isspace(LastChar)) {
    if (LastChar == '\n' || LastChar == '\r') {
      lineNo++;
      columnNo = 1;
    }
    LastChar = getc(pFile);
    columnNo++;
  }

  if (isalpha(LastChar) ||
      (LastChar == '_')) { // identifier: [a-zA-Z_][a-zA-Z_0-9]*
    IdentifierStr = LastChar;
    columnNo++;

    while (isalnum((LastChar = getc(pFile))) || (LastChar == '_')) {
      IdentifierStr += LastChar;
      columnNo++;
    }

    if (IdentifierStr == "int")
      return returnTok("int", INT_TOK);
    if (IdentifierStr == "bool")
      return returnTok("bool", BOOL_TOK);
    if (IdentifierStr == "float")
      return returnTok("float", FLOAT_TOK);
    if (IdentifierStr == "void")
      return returnTok("void", VOID_TOK);
    if (IdentifierStr == "bool")
      return returnTok("bool", BOOL_TOK);
    if (IdentifierStr == "extern")
      return returnTok("extern", EXTERN);
    if (IdentifierStr == "if")
      return returnTok("if", IF);
    if (IdentifierStr == "else")
      return returnTok("else", ELSE);
    if (IdentifierStr == "while")
      return returnTok("while", WHILE);
    if (IdentifierStr == "return")
      return returnTok("return", RETURN);
    if (IdentifierStr == "true") {
      BoolVal = true;
      return returnTok("true", BOOL_LIT);
    }
    if (IdentifierStr == "false") {
      BoolVal = false;
      return returnTok("false", BOOL_LIT);
    }

    return returnTok(IdentifierStr.c_str(), IDENT);
  }

  if (LastChar == '=') {
    NextChar = getc(pFile);
    if (NextChar == '=') { // EQ: ==
      LastChar = getc(pFile);
      columnNo += 2;
      return returnTok("==", EQ);
    } else {
      LastChar = NextChar;
      columnNo++;
      return returnTok("=", ASSIGN);
    }
  }

  if (LastChar == '{') {
    LastChar = getc(pFile);
    columnNo++;
    return returnTok("{", LBRA);
  }
  if (LastChar == '}') {
    LastChar = getc(pFile);
    columnNo++;
    return returnTok("}", RBRA);
  }
  if (LastChar == '(') {
    LastChar = getc(pFile);
    columnNo++;
    return returnTok("(", LPAR);
  }
  if (LastChar == ')') {
    LastChar = getc(pFile);
    columnNo++;
    return returnTok(")", RPAR);
  }
  if (LastChar == ';') {
    LastChar = getc(pFile);
    columnNo++;
    return returnTok(";", SC);
  }
  if (LastChar == ',') {
    LastChar = getc(pFile);
    columnNo++;
    return returnTok(",", COMMA);
  }

  if (isdigit(LastChar) || LastChar == '.') { // Number: [0-9]+.
    std::string NumStr;

    if (LastChar == '.') { // Floatingpoint Number: .[0-9]+
      do {
        NumStr += LastChar;
        LastChar = getc(pFile);
        columnNo++;
      } while (isdigit(LastChar));

      FloatVal = strtof(NumStr.c_str(), nullptr);
      return returnTok(NumStr, FLOAT_LIT);
    } else {
      do { // Start of Number: [0-9]+
        NumStr += LastChar;
        LastChar = getc(pFile);
        columnNo++;
      } while (isdigit(LastChar));

      if (LastChar == '.') { // Floatingpoint Number: [0-9]+.[0-9]+)
        do {
          NumStr += LastChar;
          LastChar = getc(pFile);
          columnNo++;
        } while (isdigit(LastChar));

        FloatVal = strtof(NumStr.c_str(), nullptr);
        return returnTok(NumStr, FLOAT_LIT);
      } else { // Integer : [0-9]+
        IntVal = strtod(NumStr.c_str(), nullptr);
        return returnTok(NumStr, INT_LIT);
      }
    }
  }

  if (LastChar == '&') {
    NextChar = getc(pFile);
    if (NextChar == '&') { // AND: &&
      LastChar = getc(pFile);
      columnNo += 2;
      return returnTok("&&", AND);
    } else {
      LastChar = NextChar;
      columnNo++;
      return returnTok("&", int('&'));
    }
  }

  if (LastChar == '|') {
    NextChar = getc(pFile);
    if (NextChar == '|') { // OR: ||
      LastChar = getc(pFile);
      columnNo += 2;
      return returnTok("||", OR);
    } else {
      LastChar = NextChar;
      columnNo++;
      return returnTok("|", int('|'));
    }
  }

  if (LastChar == '!') {
    NextChar = getc(pFile);
    if (NextChar == '=') { // NE: !=
      LastChar = getc(pFile);
      columnNo += 2;
      return returnTok("!=", NE);
    } else {
      LastChar = NextChar;
      columnNo++;
      return returnTok("!", NOT);
      ;
    }
  }

  if (LastChar == '<') {
    NextChar = getc(pFile);
    if (NextChar == '=') { // LE: <=
      LastChar = getc(pFile);
      columnNo += 2;
      return returnTok("<=", LE);
    } else {
      LastChar = NextChar;
      columnNo++;
      return returnTok("<", LT);
    }
  }

  if (LastChar == '>') {
    NextChar = getc(pFile);
    if (NextChar == '=') { // GE: >=
      LastChar = getc(pFile);
      columnNo += 2;
      return returnTok(">=", GE);
    } else {
      LastChar = NextChar;
      columnNo++;
      return returnTok(">", GT);
    }
  }

  if (LastChar == '/') { // could be division or could be the start of a comment
    LastChar = getc(pFile);
    columnNo++;
    if (LastChar == '/') { // definitely a comment
      do {
        LastChar = getc(pFile);
        columnNo++;
      } while (LastChar != EOF && LastChar != '\n' && LastChar != '\r');

      if (LastChar != EOF)
        return gettok();
    } else
      return returnTok("/", DIV);
  }

  // Check for end of file.  Don't eat the EOF.
  if (LastChar == EOF) {
    columnNo++;
    return returnTok("0", EOF_TOK);
  }

  // Otherwise, just return the character as its ascii value.
  int ThisChar = LastChar;
  std::string s(1, ThisChar);
  LastChar = getc(pFile);
  columnNo++;
  return returnTok(s, int(ThisChar));
}

static std::map<std::string, AllocaInst*> NamedValues;

// AST Nodes functions 

Value *ProgramAST::codegen() {
  for (int i = 0; i < ExternList.size(); i++) {
    if (ExternList[i])
      ExternList[i]->codegen();
  }

  // for (int i = 0; i < DeclList.size(); i++) {
  //   if (DeclList[i])
  //     DeclList[i]->codegen();
  // }

  std::cout << "testing" << std::endl;

  return nullptr;
}

std::string ProgramAST::to_string(int depth) const {
  std::string whitespace(depth, ' ');
  std::string list_elements = "";
  int cur_depth = depth;
  for (int i = 0; i < ExternList.size(); i++) {
    std::string element = (ExternList[i]) ? ExternList[i]->to_string(cur_depth) : "null";
    (i < ExternList.size() - 1) ? list_elements += element + "\n"
                           : list_elements += element;
  }
  list_elements += "\n";
  for (int i = 0; i < DeclList.size(); i++) {
    std::string element = (DeclList[i]) ? DeclList[i]->to_string(cur_depth) : "null";
    (i < DeclList.size() - 1) ? list_elements += element + "\n"
                           : list_elements += element;
  }
  return list_elements;
}


Value *BlockAST::codegen() {}

std::string BlockAST::to_string(int depth) const {
  std::string whitespace(depth, ' ');
  std::string list_elements = "";
  int cur_depth = depth;
  for (int i = 0; i < LocalDecls.size(); i++) {
    std::string element = (LocalDecls[i]) ? LocalDecls[i]->to_string(cur_depth) : "null";
    (i < LocalDecls.size() - 1) ? list_elements += element + "\n"
                           : list_elements += element;
  }
  list_elements += "\n";
  for (int i = 0; i < StmtList.size(); i++) {
    std::string element = (StmtList[i]) ? StmtList[i]->to_string(cur_depth) : "null";
    (i < StmtList.size() - 1) ? list_elements += element + "\n"
                           : list_elements += element;
  }
  return list_elements;
}

Value *LiteralASTNode::codegen() {
  switch (Tok.type) {
  case (INT_TOK):
    return ConstantInt::get(TheContext, APInt(std::stoi(Tok.lexeme), false));
  case (FLOAT_TOK):
    return ConstantFP::get(TheContext, APFloat(std::stof(Tok.lexeme)));
  case (BOOL_TOK):
    return ConstantInt::get(TheContext, APInt(std::stoi(Tok.lexeme), false));
  case (IDENT):
    Value *V = NamedValues[Tok.lexeme];

    if (!V)
      return nullptr;

    return Builder.CreateLoad(V, Tok.lexeme.c_str());

    
  }
}

std::string LiteralASTNode::to_string(int depth) const {
  // return a sting representation of this AST node
  std::string whitespace(depth, ' ');
  return whitespace + Tok.lexeme;
}

Value *VoidASTNode::codegen() {}

std::string VoidASTNode::to_string(int depth) const {
  // return a sting representation of this AST node
  std::string whitespace(depth, ' ');
  return whitespace + Tok.lexeme;
}

Value *BinaryExprAST::codegen() {

  switch (Op.type) {
  case (PLUS):
    return Builder.CreateAdd(LHS->codegen(), RHS->codegen(), "addtmp");
  case (MINUS):
    return Builder.CreateSub(LHS->codegen(), RHS->codegen(), "subtmp");
  case (ASTERIX):
    return Builder.CreateMul(LHS->codegen(), RHS->codegen(), "multmp");
  case (DIV):
    return Builder.CreateSDiv(LHS->codegen(), RHS->codegen(), "divtmp");
  case (MOD):
    return Builder.CreateSRem(LHS->codegen(), RHS->codegen(), "modtmp");
  case (EQ):
    return Builder.CreateICmpEQ(LHS->codegen(), RHS->codegen(), "eqtmp");
  case (NE):
    return Builder.CreateICmpNE(LHS->codegen(), RHS->codegen(), "netmp");
  case (LE):
    return Builder.CreateICmpULE(LHS->codegen(), RHS->codegen(), "uletmp");
  case (LT):
    return Builder.CreateICmpULT(LHS->codegen(), RHS->codegen(), "ulttmp");
  case (GE):
    return Builder.CreateICmpUGE(LHS->codegen(), RHS->codegen(), "ugetmp");
  case (GT):
    return Builder.CreateICmpUGT(LHS->codegen(), RHS->codegen(), "ugttmp");
  }
}

std::string BinaryExprAST::to_string(int depth) const {
  // return a sting representation of this AST node

  std::string whitespace(depth, ' ');
  std::string left = (LHS) ? LHS->to_string(depth + 1) : "null";
  std::string right = (RHS) ? RHS->to_string(depth + 1) : "null";
  return whitespace + "Op: " + Op.lexeme + "\n" + whitespace + left + "\n" +
         whitespace + right;
}

Value *UnaryExprAST::codegen() {
  switch (Op.type) {
  case (NOT):
    return Builder.CreateNot(Expr->codegen(), "nottmp");
  case (MINUS):
    return Builder.CreateNeg(Expr->codegen(), "negtmp");
  }
}

std::string UnaryExprAST::to_string(int depth) const {
  // return a sting representation of this AST node
  std::string whitespace(depth, ' ');
  depth += 1;
  std::string expression = (Expr) ? Expr->to_string(depth + 1) : "null";
  return whitespace + "Op: " + Op.lexeme + "\n" + expression;
}

Value *CallExprAST::codegen() {
  Function *CalleeF = TheModule->getFunction(Callee.lexeme);

  if (!CalleeF)
    return nullptr;

  if (CalleeF->arg_size() != Args.size())
    return nullptr;

  std::vector<Value *> ArgsV;

  for (int i = 0; i < Args.size(); i++) {
    ArgsV.push_back(Args[i]->codegen());
  }

  return Builder.CreateCall(CalleeF, ArgsV, "calltmp");
}

std::string CallExprAST::to_string(int depth) const {
  std::string arguments = "";
  for (int i = 0; i < Args.size(); i++) {
    std::string element = (Args[i]) ? Args[i]->to_string(depth + 1) : "null";
    (i < Args.size() - 1) ? arguments += element + ", " : arguments += element;
  }
  std::string whitespace(depth, ' ');
  return whitespace + Callee.lexeme + "(" + arguments + ")";
}

Function *PrototypeAST::codegen() {
 
  std::vector<llvm::Type*> Arguments(Args.size());

  //load correct types into prototype

  // for (int i = 0; i < Args.size(); i++) {
  //   Arguments.push_back(Args[i]->codegen());

  // }

  FunctionType *FT =
  FunctionType::get(Type::getInt32Ty(TheContext), Arguments, false);


  Function *F =
      Function::Create(FT, Function::ExternalLinkage, Name.lexeme, TheModule.get());


  //set names for all arguments
  int i = 0;
  for (auto &Arg : F->args())
    Arg.setName(Args[i++]);


  return F;
}

std::string PrototypeAST::to_string(int depth) const {
  std::string arguments = "";
  for (int i = 0; i < Args.size(); i++) {
    std::string element = (Args[i]) ? Args[i]->to_string(0) : "null";
    (i < Args.size() - 1) ? arguments += element + ", " : arguments += element;
  }
  std::string whitespace(depth, ' ');
  return whitespace + Type.lexeme + " " + Name.lexeme + "(" + arguments + ")";
}

Function *FunctionAST::codegen() {

}

std::string FunctionAST::to_string(int depth) const {
  // return a sting representation of this AST node
  std::string whitespace(depth, ' ');
  std::string prototype = (Proto) ? Proto->to_string(0) : "null";
  std::string functionbody = (Body) ? Body->to_string(depth + 1) : "null";
  return whitespace + prototype + "\n" + functionbody;
}

Value *IfAST::codegen() {
  Function *TheFunction = Builder.GetInsertBlock()->getParent();

  // generate condition
  Value *cond = Condition->codegen();

  // convert condition to bool by comparing != 0
  Value *comp = Builder.CreateICmpNE(
      cond, ConstantInt::get(TheContext, APInt(32, 0, false)), "ifcond");

  // create blocks
  BasicBlock *true_ = BasicBlock::Create(TheContext, "then", TheFunction);

  BasicBlock *false_ = BasicBlock::Create(TheContext, "else");

      BasicBlock *end_ = BasicBlock::Create(TheContext, "end");

  Builder.CreateCondBr(comp, true_, false_);

  // if true
  Builder.SetInsertPoint(true_);
  IfBody->codegen();
  Builder.CreateBr(end_);

  // if false
  TheFunction->getBasicBlockList().push_back(false_);
  Builder.SetInsertPoint(false_);
  ElseBody->codegen();

  TheFunction->getBasicBlockList().push_back(end_);
  Builder.CreateBr(end_);
  Builder.SetInsertPoint(end_);

  return nullptr;
}

std::string IfAST::to_string(int depth) const {
  // return a sting representation of this AST node
  std::string con = (Condition) ? Condition->to_string(0) : "null";
  std::string ifBody = (IfBody) ? IfBody->to_string(depth + 1) : "null";
  std::string elseBody = (ElseBody) ? ElseBody->to_string(depth + 1) : "null";
  std::string whitespace(depth, ' ');
  return whitespace + "if (" + con + ")\n " + ifBody + "\n" + whitespace +
         "else \n" + whitespace + elseBody;
}

Value *WhileAST::codegen() {
  Function *TheFunction = Builder.GetInsertBlock()->getParent();

  // loop block
  BasicBlock *loop = BasicBlock::Create(TheContext, "loop", TheFunction);

  // block afer loop
  BasicBlock *exit = BasicBlock::Create(TheContext, "end");

  // generate condition and generate branch which decides to loop or exit
  // based off of condtion
  Value *cond = Condition->codegen();

  Value *comp = Builder.CreateICmpNE(
      cond, ConstantInt::get(TheContext, APInt(32, 0, false)), "whilecond");
  Builder.CreateCondBr(comp, loop, exit);

  // work inside body
  Builder.SetInsertPoint(loop);

  Body->codegen();

  Value *endcond = Condition->codegen();

  // evaluate condtion and choose either to loop again or exit
  Value *endcomp = Builder.CreateICmpNE(
      endcond, ConstantInt::get(TheContext, APInt(32, 0, false)), "whilecond");
  Builder.CreateCondBr(endcomp, loop, exit);

  // leave loop
  TheFunction->getBasicBlockList().push_back(exit);
  Builder.CreateBr(exit);
  Builder.SetInsertPoint(exit);

  return nullptr;
}

std::string WhileAST::to_string(int depth) const {
  // return a sting representation of this AST node
  std::string con = (Condition) ? Condition->to_string(0) : "null";
  std::string whilebody = (Body) ? Body->to_string(depth + 1) : "null";
  std::string whitespace(depth, ' ');
  return whitespace + "while (" + con + ") \n" + whilebody;
}

Value *ReturnAST::codegen() { 

  return nullptr; 
}

std::string ReturnAST::to_string(int depth) const {
  // return a sting representation of this AST node
  std::string returnbody = (Body) ? Body->to_string(0) : "null";
  std::string whitespace(depth, ' ');
  return whitespace + "return " + returnbody;
}

Value *VarDeclAST::codegen() { return nullptr; }

std::string VarDeclAST::to_string(int depth) const {
  // return a sting representation of this AST node
  std::string whitespace(depth, ' ');
  return whitespace + Type.lexeme + " " + Name.lexeme;
}

Value *VarAssignAST::codegen() {

  Value *Val = Expr->codegen();

  if (!Val)
    return nullptr;

  Value *Variable = NamedValues[Name.lexeme];

  if (!Variable)
    return nullptr;

  Builder.CreateStore(Val, Variable);
  
  return Val;
}

std::string VarAssignAST::to_string(int depth) const {
  // return a sting representation of this AST node
  std::string expression = (Expr) ? Expr->to_string(depth + 1) : "null";
  std::string whitespace(depth, ' ');
  return whitespace + Name.lexeme + " assigned\n" + expression;
}

//===----------------------------------------------------------------------===//
// Parser
//===----------------------------------------------------------------------===//

/// CurTok/getNextToken - Provide a simple token buffer.  CurTok is the
/// current token the parser is looking at.  getNextToken reads another token
/// from the lexer and updates CurTok with its results.
static TOKEN CurTok;
static std::deque<TOKEN> tok_buffer;

static TOKEN getNextToken() {

  if (tok_buffer.size() == 0)
    tok_buffer.push_back(gettok());

  TOKEN temp = tok_buffer.front();
  tok_buffer.pop_front();

  return CurTok = temp;
}

static void putBackToken(TOKEN tok) { tok_buffer.push_front(tok); }

void match(int word) {
  if (CurTok.type == word) {
    // std::cerr << "matched " << CurTok.lexeme << std::endl;
    getNextToken();
  } else {
    std::cerr << "Expected token " << word << " but got " << CurTok.type
              << std::endl;
    exit(0);
  }
}

//===----------------------------------------------------------------------===//
// Recursive Descent Parser - Function call for each production
//===----------------------------------------------------------------------===//

std::unique_ptr<ASTNode> parser() {
  auto program_scope = program();

  switch (CurTok.type) {
  case (EOF_TOK):
    return std::move(program_scope);
  default:
    // error
    exit(0);
  }
}

// program -> extern_list decl_list | decl_list
std::unique_ptr<ASTNode> program() {
  std::vector<std::unique_ptr<ASTNode>> extern_vector;
  std::vector<std::unique_ptr<ASTNode>> decl_vector;

  switch (CurTok.type) {
  case (EXTERN):
    extern_list(extern_vector);
  case (BOOL_TOK):
  case (FLOAT_TOK):
  case (INT_TOK):
  case (VOID_TOK): {
    decl_list(decl_vector);
    auto scope = std::make_unique<ProgramAST>(std::move(extern_vector),
                                            std::move(decl_vector));
    return std::move(scope);
  }
  default:
    std::cerr << "Expected extern or bool or float or int or void "
              << std::endl;

    exit(0);
  }
}

// extern_list -> extern extern_list_prime
void extern_list(std::vector<std::unique_ptr<ASTNode>> &list) {
  auto _extern_ = extern_();
  list.push_back(std::move(_extern_));
  extern_list_prime(list);
}

// extern_list_prime -> extern extern_list_prime | epsilon
void extern_list_prime(std::vector<std::unique_ptr<ASTNode>> &list) {

  switch (CurTok.type) {
  case (INT_TOK):
  case (FLOAT_TOK):
  case (BOOL_TOK):
  case (VOID_TOK):
    return;
  case (EXTERN):
    auto _extern_ = extern_();
    list.push_back(std::move(_extern_));
    extern_list_prime(list);
    return;
  }

  std::cerr << "Expected extern or int or float or bool or void" << std::endl;
  exit(0);
}

// extern -> "extern" type_spec IDENT "(" params ")" ";"
std::unique_ptr<ASTNode> extern_() {
  match(EXTERN);
  auto type = type_spec();

  TOKEN identifier = CurTok;
  match(IDENT); // consume identifier

  match(LPAR); // consumer (

  auto parameters = params();
  match(RPAR);
  match(SC);
  auto prototype =
      std::make_unique<PrototypeAST>(type, identifier, std::move(parameters));

  return std::move(prototype);
}

// decl_list -> decl decl_list_prime
void decl_list(std::vector<std::unique_ptr<ASTNode>> &list) {

  auto declaration = decl();
  list.push_back(std::move(declaration));
  decl_list_prime(list);
}

// decl_list -> decl decl_list_prime | epsilon
void decl_list_prime(std::vector<std::unique_ptr<ASTNode>> &list) {
  if (CurTok.type != EOF_TOK) {
    auto declaration = decl();
    list.push_back(std::move(declaration));
    decl_list_prime(list);
  }
}

// lookahead function used by decl to decide between var_decl and fun_decl
TOKEN peekll3() {
  TOKEN old = CurTok;

  TOKEN lookahead1 = getNextToken();
  TOKEN lookahead2 = getNextToken();

  TOKEN temp = lookahead2; // token to return for lookahead

  putBackToken(lookahead2);
  putBackToken(lookahead1);

  CurTok = old;

  return temp;
}

// decl -> var_decl | fun_decl
std::unique_ptr<ASTNode> decl() {
  switch (CurTok.type) {
  case (VOID_TOK): // only fun_decl uses "void"
    return fun_decl();
  case (INT_TOK):
  case (FLOAT_TOK):
  case (BOOL_TOK):

    TOKEN lookahead = peekll3();
    switch (lookahead.type) {
    case (SC): // a ";" follows an identifier in a variable declaration
      return var_decl();
    case (LPAR): // a "(" follows an identifier in a function declaration
      return fun_decl();
    }
  }
  std::cerr << "expected decl() but got " << CurTok.lexeme << std::endl;
  exit(0);
}

// var_decl -> var_type IDENT ";"
std::unique_ptr<VarDeclAST> var_decl() {
  TOKEN type = var_type();
  TOKEN name = CurTok;
  auto variable_declaration = std::make_unique<VarDeclAST>(type, name);
  match(IDENT); // consume identifier
  match(SC);    // consume ;

  return std::move(variable_declaration);
}

std::unique_ptr<FunctionAST> fun_decl() {
  auto type = type_spec();
  TOKEN identifier = CurTok;

  match(IDENT); // consume identifier
  match(LPAR);  // consumer identifier
  std::vector<std::unique_ptr<ASTNode>> parameters = params();

  match(RPAR);

  // build function definition
  auto scope = block();
  auto prototype =
      std::make_unique<PrototypeAST>(type, identifier, std::move(parameters));
  auto function =
      std::make_unique<FunctionAST>(std::move(prototype), std::move(scope));

  return std::move(function);
}

TOKEN var_type() {
  switch (CurTok.type) {
  case (INT_TOK):
  case (FLOAT_TOK):
  case (BOOL_TOK):
    TOKEN type = CurTok;
    getNextToken(); // consume type token
    return type;
  }
  std::cerr << "Expected extern or int or float or bool or void" << std::endl;
  exit(0);
}

TOKEN type_spec() {
  if (CurTok.type == VOID_TOK) {
    TOKEN type = CurTok;
    getNextToken(); // consume type token
    return type;
  }
  return var_type();
}

std::vector<std::unique_ptr<ASTNode>> params() {

  std::vector<std::unique_ptr<ASTNode>> parameter_list;
  switch (CurTok.type) {
  case (RPAR):
    // return empty list to signify no arguments
    return std::move(parameter_list);
  case (BOOL_TOK):
  case (FLOAT_TOK):
  case (INT_TOK):

    param_list(parameter_list);
    return std::move(parameter_list);
  case (VOID_TOK):
    TOKEN tok = CurTok;
    match(VOID_TOK);
    parameter_list.push_back(std::move(std::make_unique<VoidASTNode>(tok)));
    return std::move(parameter_list);
  }
  // error
  std::cerr << "expected params()" << std::endl;
  exit(0);
}

void param_list(std::vector<std::unique_ptr<ASTNode>> &list) {
  auto parameter = param();
  list.push_back(std::move(parameter));
  param_list_prime(list);
}

void param_list_prime(std::vector<std::unique_ptr<ASTNode>> &list) {
  if (CurTok.type != RPAR) {
    match(COMMA);
    auto parameter = param();
    list.push_back(std::move(parameter));
    if (CurTok.type == COMMA) {
      param_list_prime(list);
    }
  }
}

std::unique_ptr<ASTNode> param() {
  TOKEN type = var_type();
  TOKEN name = CurTok;
  match(IDENT);
  auto parameter = std::make_unique<VarDeclAST>(type, name);
  return std::move(parameter);
}

std::unique_ptr<ASTNode> block() {
  match(LBRA);
  std::vector<std::unique_ptr<ASTNode>> local_declaration_list;
  std::vector<std::unique_ptr<ASTNode>> statement_list;

  // build lists
  local_decls(local_declaration_list);
  stmt_list(statement_list);

  // make copies of the lists
  //  auto local_declarations = local_declaration_list;
  //  auto statements = statement_list;

  auto scope = std::make_unique<BlockAST>(std::move(local_declaration_list),
                                          std::move(statement_list));

  match(RBRA);

  return std::move(scope);
}

void local_decls(std::vector<std::unique_ptr<ASTNode>> &list) {
  local_decls_prime(list);
}

void local_decls_prime(std::vector<std::unique_ptr<ASTNode>> &list) {

  switch (CurTok.type) {
  case (NE):
  case (MOD):
  case (AND):
  case (ASTERIX):
  case (PLUS):
  case (MINUS):
  case (DIV):
  case (SC):
  case (LT):
  case (LE):
  case (GT):
  case (GE):
  case (NOT):
  case (LPAR):
  case (IF):
  case (RETURN):
  case (WHILE):
  case (LBRA):
  case (RBRA):
  case (BOOL_LIT):
  case (FLOAT_LIT):
  case (IDENT):
  case (INT_LIT):
    return;
  }

  auto local_declaration = local_decl();
  list.push_back(std::move(local_declaration));
  local_decls(list);
}

std::unique_ptr<ASTNode> local_decl() {
  TOKEN type = var_type();
  TOKEN identifier = CurTok;
  match(IDENT);
  auto variable_declaration = std::make_unique<VarDeclAST>(type, identifier);
  match(SC);
  return std::move(variable_declaration);
}

void stmt_list(std::vector<std::unique_ptr<ASTNode>> &list) {
  stmt_list_prime(list);
}

void stmt_list_prime(std::vector<std::unique_ptr<ASTNode>> &list) {
  if (CurTok.type != RBRA) {
    auto statement = stmt();
    list.push_back(std::move(statement));
    stmt_list(list);
  }
}

std::unique_ptr<ASTNode> stmt() {
  switch (CurTok.type) {
  case (NOT):
  case (LPAR):
  case (MINUS):
  case (SC):
  case (BOOL_LIT):
  case (FLOAT_LIT):
  case (IDENT):
  case (INT_LIT):

    return std::move(expr_stmt());
  case (LBRA):
    return std::move(block());
  case (IF):
    return std::move(if_stmt());
  case (WHILE):
    return std::move(while_stmt());
  case (RETURN):
    return std::move(return_stmt());
  }
  std::cerr << "Expected stmt" << std::endl;
  exit(0);
}

std::unique_ptr<ASTNode> expr_stmt() {
  switch (CurTok.type) {
  case (NOT):
  case (LPAR):
  case (MINUS):
  case (SC):
  case (BOOL_LIT):
  case (FLOAT_LIT):
  case (IDENT):
  case (INT_LIT):
    auto expression = expr();
    match(SC);
    return std::move(expression);
  }
  match(SC);
  return nullptr;
}

std::unique_ptr<ASTNode> while_stmt() {
  match(WHILE);
  match(LPAR);
  auto expression = expr();
  match(RPAR);
  auto statement = stmt();

  auto while_statement =
      std::make_unique<WhileAST>(std::move(expression), std::move(statement));

  return std::move(while_statement);
}

std::unique_ptr<ASTNode> if_stmt() {
  match(IF);
  match(LPAR);
  auto expression = expr();
  match(RPAR);
  auto if_block = block();
  auto else_statement = else_stmt();

  auto if_statement = std::make_unique<IfAST>(
      std::move(expression), std::move(if_block), std::move(else_statement));

  return std::move(if_statement);
}

std::unique_ptr<ASTNode> else_stmt() {
  switch (CurTok.type) {
  case (NOT):
  case (LPAR):
  case (MINUS):
  case (SC):
  case (IF):
  case (RETURN):
  case (WHILE):
  case (LBRA):
  case (RBRA):
  case (BOOL_LIT):

  case (FLOAT_LIT):
  case (IDENT):
  case (INT_LIT):
    return nullptr;
  }
  match(ELSE);
  auto else_block = block();
  return std::move(else_block);
}

std::unique_ptr<ASTNode> return_stmt() {
  match(RETURN);
  auto return_body = return_stmt_B();
  auto return_statement = std::make_unique<ReturnAST>(std::move(return_body));

  return std::move(return_statement);
}

std::unique_ptr<ASTNode> return_stmt_B() {
  switch (CurTok.type) {
  case (NOT):
  case (LPAR):
  case (MINUS):
  case (BOOL_LIT):
  case (FLOAT_LIT):
  case (IDENT):
  case (INT_LIT):
    auto expression = expr();
    match(SC);
    return std::move(expression);
  }
  match(SC);
  return nullptr;
}

TOKEN peekll2() {
  TOKEN old = CurTok;
  TOKEN lookahead = getNextToken();
  TOKEN temp = lookahead; // token to return for lookahead
  putBackToken(lookahead);
  CurTok = old;
  return temp;
}

// will need a lookaheads due to IDENT
std::unique_ptr<ASTNode> expr() {
  // determine if it is a variable assignment or a potential variable
  // statement or function call
  if (CurTok.type == IDENT) {

    TOKEN lookahead = peekll2();
    if (lookahead.type == ASSIGN) {
      TOKEN name = CurTok;

      match(IDENT);
      match(ASSIGN);

      auto expression = expr();

      auto variable_assignment =
          std::make_unique<VarAssignAST>(name, std::move(expression));

      return std::move(variable_assignment);
    }
  }
  switch (CurTok.type) {
  case (NOT):
  case (LPAR):
  case (MINUS):
  case (BOOL_LIT):
  case (FLOAT_LIT):
  case (INT_LIT):
  case (IDENT):
    auto expression = or_val();
    return std::move(expression);
  }

  std::cerr << "expected a variable assignment or expression" << std::endl;
  exit(0);
}

std::unique_ptr<ASTNode> or_val() {
  auto lhs = and_val();
  while (CurTok.type == OR) {
    TOKEN op = CurTok;
    getNextToken();
    auto rhs = and_val();
    lhs = std::make_unique<BinaryExprAST>(op, std::move(lhs), std::move(rhs));
  }
  return or_val_prime(std::move(lhs));
}

std::unique_ptr<ASTNode> or_val_prime(std::unique_ptr<ASTNode> lhs) {
  TOKEN op;

  switch (CurTok.type) {
  case (RPAR):
  case (SC):
  case (COMMA):
    return std::move(lhs);
  case (OR):
    op = CurTok;
    getNextToken();

    auto rhs = and_val();
    rhs = or_val_prime(std::move(rhs));
    auto bin_op =
        std::make_unique<BinaryExprAST>(op, std::move(lhs), std::move(rhs));
    return std::move(bin_op);
  }
  // error
  std::cerr << "expected expression" << std::endl;
  exit(0);
}

std::unique_ptr<ASTNode> and_val() {

  auto lhs = eq_val();
  while (CurTok.type == AND) {
    TOKEN op = CurTok;
    getNextToken();
    auto rhs = eq_val();
    lhs = std::make_unique<BinaryExprAST>(op, std::move(lhs), std::move(rhs));
  }
  return and_val_prime(std::move(lhs));
}

std::unique_ptr<ASTNode> and_val_prime(std::unique_ptr<ASTNode> lhs) {
  TOKEN op;
  switch (CurTok.type) {
  case (RPAR):
  case (SC):
  case (OR):
  case (COMMA):
    return std::move(lhs);
  case (AND):
    op = CurTok;
    getNextToken();

    auto rhs = eq_val();
    rhs = and_val_prime(std::move(rhs));

    auto bin_op =
        std::make_unique<BinaryExprAST>(op, std::move(lhs), std::move(rhs));
    return std::move(bin_op);
  }
  std::cerr << "and_val" << std::endl;
  exit(0);
}

std::unique_ptr<ASTNode> eq_val() {
  auto lhs = comp_val();

  while (CurTok.type == EQ || CurTok.type == NE) {
    TOKEN op = CurTok;
    getNextToken();
    auto rhs = comp_val();
    lhs = std::make_unique<BinaryExprAST>(op, std::move(lhs), std::move(rhs));
  }

  return eq_val_prime(std::move(lhs));
}

std::unique_ptr<ASTNode> eq_val_prime(std::unique_ptr<ASTNode> lhs) {
  TOKEN op;
  switch (CurTok.type) {
  case (AND):
  case (RPAR):
  case (SC):
  case (OR):
  case (COMMA):
    return std::move(lhs);
  case (EQ):
  case (NE):
    op = CurTok;
    getNextToken();

    auto rhs = comp_val();
    rhs = eq_val_prime(std::move(rhs));

    auto bin_op =
        std::make_unique<BinaryExprAST>(op, std::move(lhs), std::move(rhs));
    return std::move(bin_op);
  }
  std::cerr << "eq_val" << std::endl;
  exit(0);
}

std::unique_ptr<ASTNode> comp_val() {
  auto lhs = add_val();
  while (CurTok.type == LE || CurTok.type == LT || CurTok.type == GE ||
         CurTok.type == GT) {
    TOKEN op = CurTok;
    getNextToken();
    auto rhs = add_val();
    lhs = std::make_unique<BinaryExprAST>(op, std::move(lhs), std::move(rhs));
  }
  return comp_val_prime(std::move(lhs));
}

std::unique_ptr<ASTNode> comp_val_prime(std::unique_ptr<ASTNode> lhs) {
  TOKEN op;
  switch (CurTok.type) {
  case (NE):
  case (AND):
  case (RPAR):
  case (SC):
  case (EQ):
  case (OR):
  case (COMMA):
    return std::move(lhs);
  case (LE):
  case (LT):
  case (GE):
  case (GT):
    op = CurTok;
    getNextToken();

    auto rhs = add_val();
    rhs = comp_val_prime(std::move(rhs));

    auto bin_op =
        std::make_unique<BinaryExprAST>(op, std::move(lhs), std::move(rhs));
    return std::move(bin_op);
  }
  std::cerr << "comp_val" << std::endl;
  exit(0);
}

std::unique_ptr<ASTNode> add_val() {
  auto lhs = mul_val();

  while (CurTok.type == PLUS || CurTok.type == MINUS) {
    TOKEN op = CurTok;
    getNextToken();
    auto rhs = mul_val();
    lhs = std::make_unique<BinaryExprAST>(op, std::move(lhs), std::move(rhs));
  }
  return add_val_prime(std::move(lhs));
}

std::unique_ptr<ASTNode> add_val_prime(std::unique_ptr<ASTNode> lhs) {
  TOKEN op;
  switch (CurTok.type) {
  case (NE):
  case (AND):
  case (RPAR):
  case (SC):
  case (LT):
  case (LE):
  case (EQ):
  case (GT):
  case (GE):
  case (OR):
  case (COMMA):
    return std::move(lhs);
  case (PLUS):
  case (MINUS):
    op = CurTok;
    getNextToken();
    auto rhs = mul_val();
    std::cout << ((rhs) ? rhs->to_string(0) : "null") << std::endl;
    rhs = add_val_prime(std::move(rhs));
    std::cout << ((rhs) ? rhs->to_string(0) : "null") << std::endl;

    std::cout << ((lhs) ? lhs->to_string(0) : "null") << std::endl;

    auto bin_op =
        std::make_unique<BinaryExprAST>(op, std::move(lhs), std::move(rhs));
    return std::move(bin_op);
  }
  std::cerr << "add_val" << std::endl;
  exit(0);
}

std::unique_ptr<ASTNode> mul_val() {
  auto lhs = unary();
  while (CurTok.type == ASTERIX || CurTok.type == DIV || CurTok.type == MOD) {
    TOKEN op = CurTok;
    getNextToken();
    auto rhs = unary();
    lhs = std::make_unique<BinaryExprAST>(op, std::move(lhs), std::move(rhs));
  }
  return mul_val_prime(std::move(lhs));
}

std::unique_ptr<ASTNode> mul_val_prime(std::unique_ptr<ASTNode> lhs) {
  TOKEN op;

  switch (CurTok.type) {
  case (NE):
  case (AND):
  case (RPAR):
  case (PLUS):
  case (MINUS):
  case (SC):
  case (LT):
  case (LE):
  case (EQ):
  case (GT):
  case (GE):
  case (OR):
  case (COMMA):
    return std::move(lhs);
  case (ASTERIX):
  case (DIV):
  case (MOD):
    op = CurTok;
    getNextToken();
    auto rhs = unary();
    rhs = mul_val_prime(std::move(rhs));
    auto bin_op =
        std::make_unique<BinaryExprAST>(op, std::move(lhs), std::move(rhs));
    return std::move(bin_op);
  }
  std::cerr << "mul_val" << std::endl;
  exit(0);
}

std::unique_ptr<ASTNode> unary() {
  switch (CurTok.type) {
  case (LPAR):
  case (BOOL_LIT):
  case (FLOAT_LIT):
  case (IDENT):
  case (INT_LIT): {
    auto identifier = identifiers();
    return std::move(identifier);
  }
  case (NOT):
  case (MINUS):
    TOKEN op = CurTok;
    getNextToken();
    auto unary_expression =
        std::make_unique<UnaryExprAST>(op, std::move(unary()));

    return std::move(unary_expression);
  }
  std::cerr << "unary" << std::endl;
  exit(0);
}

std::unique_ptr<ASTNode> identifiers() {
  switch (CurTok.type) {
  case (IDENT): {
    auto identifier = identifiers_B();
    return std::move(identifier);
  }
  case (INT_LIT):
  case (FLOAT_LIT):
  case (BOOL_LIT): {
    TOKEN tok = CurTok;
    getNextToken();
    auto result = std::make_unique<LiteralASTNode>(tok);
    return std::move(result);
  }
  case (LPAR):
    match(LPAR);
    auto expression = expr();
    match(RPAR);
    return std::move(expression);
  }

  std::cerr << "expected identifier" << std::endl;
  exit(0);
}

std::unique_ptr<ASTNode> identifiers_B() {
  TOKEN identifier = CurTok;
  getNextToken();
  switch (CurTok.type) {
  case (NE):
  case (AND):
  case (RPAR):
  case (PLUS):
  case (MINUS):
  case (SC):
  case (LT):
  case (LE):
  case (EQ):
  case (GT):
  case (GE):
  case (OR):
  case (COMMA):
  case (ASTERIX):
  case (DIV):
  case (MOD): {
    auto result = std::make_unique<LiteralASTNode>(identifier);
    return std::move(result);
  }
  case (LPAR):
    // function call
    TOKEN callee = identifier;

    match(LPAR);
    std::vector<std::unique_ptr<ASTNode>> arguments = args();

    match(RPAR);
    auto function_call =
        std::make_unique<CallExprAST>(callee, std::move(arguments));
    return std::move(function_call);
  }

  std::cerr << "expected (" << std::endl;
  exit(0);
}

std::vector<std::unique_ptr<ASTNode>> args() {
  std::vector<std::unique_ptr<ASTNode>> argument_list;
  // if not the follow case, we will populate the list otherwise we simply
  // return an empty list to show we have no args
  if (CurTok.type != RPAR) {
    arg_list(argument_list);
  }

  return std::move(argument_list);
}

void arg_list(std::vector<std::unique_ptr<ASTNode>> &list) {
  auto expression = expr();
  list.push_back(std::move(expression));

  arg_list_prime(list);
}

void arg_list_prime(std::vector<std::unique_ptr<ASTNode>> &list) {
  if (CurTok.type != RPAR) {
    match(COMMA);
    auto expression = expr();
    list.push_back(std::move(expression));
    arg_list_prime(list);
  }
}

// //===----------------------------------------------------------------------===//
// // AST Printer
// //===----------------------------------------------------------------------===//

inline llvm::raw_ostream &operator<<(llvm::raw_ostream &os,
                                     const ASTNode &ast) {
  os << ast.to_string(0);
  return os;
}

//===----------------------------------------------------------------------===//
// Main driver code.
//===----------------------------------------------------------------------===//

int main(int argc, char **argv) {
  if (argc == 2) {
    pFile = fopen(argv[1], "r");
    if (pFile == NULL)
      perror("Error opening file");
  } else {
    std::cout << "Usage: ./code InputFile\n";
    return 1;
  }

  // initialize line number and column numbers to zero
  lineNo = 1;
  columnNo = 1;

  // get the first token
  getNextToken();
  // while (CurTok.type != EOF_TOK) {
  //   fprintf(stderr, "Token: %s with type %d\n", CurTok.lexeme.c_str(),
  //           CurTok.type);
  //   getNextToken();
  // }
  // fprintf(stderr, "Lexer Finished\n");

  // Make the module, which holds all the code.
  TheModule = std::make_unique<Module>("mini-c", TheContext);

  // Run the parser now.
  auto test = parser();
  // llvm::outs() << test << "\n";
  std::cout << test->to_string(0) << std::endl;

  test->codegen();
  fprintf(stderr, "Parsing Finished\n");

  //********************* Start printing final IR **************************
  // Print out all of the generated code into a file called output.ll
  auto Filename = "output.ll";
  std::error_code EC;
  raw_fd_ostream dest(Filename, EC, sys::fs::OF_None);

  if (EC) {
    errs() << "Could not open file: " << EC.message();
    return 1;
  }
  // TheModule->print(errs(), nullptr); // print IR to terminal
  TheModule->print(dest, nullptr);
  //********************* End printing final IR ****************************

  fclose(pFile); // close the file that contains the code that was parsed
  return 0;
}