#include "codegen.hpp"
#include "ast_node.hpp"

using namespace llvm;
using namespace llvm::sys;

LLVMContext TheContext;
IRBuilder<> Builder(TheContext);
std::unique_ptr<Module> TheModule;

std::queue<std::tuple<TOKEN, std::string>> WarningQueue;  // queue for storing warnings which is printed after IR generated
std::vector<std::map<std::string, AllocaInst *>> Scopes;  // vector of scopes
std::map<std::string, GlobalVariable *> GlobalVariables;  // global scope
int return_flag = 0;  //flag to see if return statement has been found
/**
 * AllocaInst*CreateEntryBlockAlloca 
 * Create alloca entries 
 */
AllocaInst *CreateEntryBlockAlloca(Function *TheFunction,
                                   const std::string &VarName,
                                   llvm::Type *type) {
  IRBuilder<> TmpB(&TheFunction->getEntryBlock(),
                   TheFunction->getEntryBlock().begin());

  return TmpB.CreateAlloca(type, 0, VarName.c_str());
}
/**
 * Value*Casting 
 * 
 * @param  {Type*} VarType    : Destination type
 * @param  {Value*} NewVal    : Value we want to cast
 * @param  {TOKEN} tok        : token for warnings
 * @param  {bool} should_warn : determining if cast should give warning
 */
Value *Casting(Type *VarType, Value *NewVal, TOKEN tok, bool should_warn) {
  TOKEN t = tok; //token used for warnings
  //convert expression into 32 bit int
  if (VarType->isIntegerTy(32)) {
    if (NewVal->getType()->isIntegerTy(32)) { 
      return NewVal;
    } else if (NewVal->getType()->isIntegerTy(1)) {
      if (should_warn)
        WarningQueue.push(std::tuple<TOKEN, std::string>(
            tok, "implicit conversion from boolean to integer"));

      return Builder.CreateIntCast(NewVal, Type::getInt32Ty(TheContext), false,
                                   "btoi32");
    } else if (NewVal->getType()->isFloatTy()) {
      if (should_warn)
        WarningQueue.push(std::tuple<TOKEN, std::string>(
            tok, "implicit conversion from float to integer"));

      return Builder.CreateFPToSI(NewVal, Type::getInt32Ty(TheContext),
                                  "ftoi32");
    }
    // convert expression into boolean
  } else if (VarType->isIntegerTy(1)) {
    if (NewVal->getType()->isIntegerTy(32)) {
      if (should_warn)
        WarningQueue.push(std::tuple<TOKEN, std::string>(
            tok, "implicit conversion from integer to boolean"));

      return Builder.CreateICmpNE(
          NewVal, ConstantInt::get(Type::getInt32Ty(TheContext), false),
          "i32tob");
    } else if (NewVal->getType()->isIntegerTy(1)) {

      return NewVal;
    } else if (NewVal->getType()->isFloatTy()) {
      if (should_warn)
        WarningQueue.push(std::tuple<TOKEN, std::string>(
            tok, "implicit conversion from float to boolean"));

      return Builder.CreateFCmpONE(
          NewVal, ConstantFP::get(TheContext, APFloat(0.0f)), "ftob");
    }
    // convert expression to floating point
  } else if (VarType->isFloatTy()) {
    if (NewVal->getType()->isIntegerTy(32)) {
      if (should_warn)
        WarningQueue.push(std::tuple<TOKEN, std::string>(
            tok, "implicit conversion from integer to float"));

      return Builder.CreateSIToFP(NewVal, Type::getFloatTy(TheContext),
                                  "i32tof");
    } else if (NewVal->getType()->isIntegerTy(1)) {
      if (should_warn)
        WarningQueue.push(std::tuple<TOKEN, std::string>(
            tok, "implicit conversion from boolean to float"));

      return Builder.CreateSIToFP(NewVal, Type::getFloatTy(TheContext),
                                  "i1tof");
    } else if (NewVal->getType()->isFloatTy()) {
      return NewVal;
    }
  }

  return nullptr;
}

/**
 * Value*ProgramAST::codegen 
 * codegen externs and declarations
 */
Value *ProgramAST::codegen() {
  for (int i = 0; i < ExternList.size(); i++) { //codegen externs
    if (ExternList[i])
      ExternList[i]->codegen();
  }

  for (int i = 0; i < DeclList.size(); i++) { //codegen declarations
    if (DeclList[i])
      DeclList[i]->codegen();
  }
  LogWarnings();  //print out warnings
  return nullptr;
}

/**
 * Value*BlockAST::codegen 
 * codegen local declarations and statements
 */
Value *BlockAST::codegen() {
  llvm::Type *FuncReturnType = Builder.getCurrentFunctionReturnType();  //get function return type
  Scopes.push_back(std::map<std::string, AllocaInst *>());  //new scope per block
  for (int i = 0; i < LocalDecls.size(); i++) { //codegen local declarations
    if (LocalDecls[i])
      LocalDecls[i]->codegen();
  }

  for (int i = 0; i < StmtList.size(); i++) { //codegen statements

    if (StmtList[i]) {
      //cast statement into return ast and check if null and if not null, we generate return statement and terminate codegen early
      auto StmtToReturn = dynamic_cast<ReturnAST *>(StmtList[i].get()); 
      if (StmtToReturn) {
        StmtToReturn->codegen();
        break;
      }
      StmtList[i]->codegen();
    }
  }
  //we have exhausted all statements for this block, but we need to check if a return statement was found and if not
  //check if function is void
  if (Scopes.size() == 2 && FuncReturnType->isVoidTy()) { //scopes.size() == 2 means we are in a function block
    if (return_flag != 1) { //no returns found but this is a void function so we generate retvoid 
      Builder.CreateRetVoid();
    }
  } else if (Scopes.size() == 2 && !(FuncReturnType->isVoidTy())) { // non void function
    if (return_flag != 1) { // no return found for non void function so semantic error
      TOKEN tok = Tok;
      LogSemanticError(tok, "no return for non-void function");
    }
  }
  Scopes.pop_back();  //we are finished with this scope
  return nullptr;
}
/**
 * Value*LiteralASTNode::codegen 
 * code gen for literals 
 */
Value *LiteralASTNode::codegen() {
  switch (Tok.type) {
  case (INT_LIT):
    return ConstantInt::get(TheContext, APInt(32, std::stoi(Tok.lexeme), true));  //return integer literal by converting token lexeme to llvm constant
  case (FLOAT_LIT): 
    return ConstantFP::get(TheContext, APFloat(std::stof(Tok.lexeme))); //return float literal by converting token lexeme to llvm constant

  case (BOOL_LIT): {
    
    int bool_ = (Tok.lexeme == "true") ? 1 : 0; //lexeme will be of of string true or false but we need to convert to numerical representation
    return ConstantInt::get(TheContext, APInt(1, bool_, false)); //return bool literal by converting token lexeme to llvm constant
  }
  case (IDENT): //identifiers

    AllocaInst *V;

    //check scopes first to see if variable exists
    for (int i = Scopes.size() - 1; i >= 0; i--) {
      if (Scopes[i].count(Tok.lexeme) > 0) {
        V = Scopes[i][Tok.lexeme];
        break;
      }
    }
    //if not found in any scope, check global variables
    if (!V) {

      GlobalVariable *G = GlobalVariables[Tok.lexeme];
      if (!G) { //variable not found, semantic error
        TOKEN t = Tok;
        LogSemanticError(t,
                         "use of undeclared identifier \"" + t.lexeme + "\"");
      }
      return Builder.CreateLoad(G->getValueType(), G, Tok.lexeme);  //variable found as global variable, convert global variable to value pointer
    } else {
      return Builder.CreateLoad(V->getAllocatedType(), V, Tok.lexeme); //variable found in scopes, convert alloca to value pointer
    }
  }

  return nullptr;
}


Value *LazyOr(std::unique_ptr<ASTNode> lhs, std::unique_ptr<ASTNode> rhs, TOKEN tok) {
  Function *TheFunction = Builder.GetInsertBlock()->getParent();
  Value *truth = ConstantInt::get(TheContext, APInt(1, 0, false));
  Value *cond = Casting(Type::getInt1Ty(TheContext) ,lhs->codegen(), tok, false);


  Value *comp = Builder.CreateICmpNE(
      cond, ConstantInt::get(TheContext, APInt(1, 0, false)), "lazyorleft");

  BasicBlock *rhs_ = BasicBlock::Create(TheContext, "orlhsf", TheFunction);
  BasicBlock *true_ = BasicBlock::Create(TheContext, "orlhst");
  BasicBlock *end_ = BasicBlock::Create(TheContext, "orend");

  //if left true then jump to true_ branch which sets truth to true
  // otherwise go check if rhs is true
  Builder.CreateCondBr(comp, true_, rhs_);

  Builder.SetInsertPoint(rhs_);
  Value *right_cond = Casting(Type::getInt1Ty(TheContext) ,rhs->codegen(), tok, false);
  Value *right_comp = Builder.CreateICmpNE(
      right_cond, ConstantInt::get(TheContext, APInt(1, 0, false)), "lazyorright");
  //check if right is true and if so, then set truth to true or just end straight away
  Builder.CreateCondBr(right_comp, true_, end_);

  TheFunction->getBasicBlockList().push_back(true_);
  Builder.SetInsertPoint(true_);
  truth = ConstantInt::get(TheContext, APInt(1, 1, false));

  TheFunction->getBasicBlockList().push_back(end_);
  Builder.CreateBr(end_);
  Builder.SetInsertPoint(end_);

  return truth;

}

Value *LazyAnd(std::unique_ptr<ASTNode> lhs, std::unique_ptr<ASTNode> rhs, TOKEN tok) {
  Function *TheFunction = Builder.GetInsertBlock()->getParent();
  Value *truth = ConstantInt::get(TheContext, APInt(1, 0, false));
  Value *cond = Casting(Type::getInt1Ty(TheContext) ,lhs->codegen(), tok, false);
  std::cout << "over here?" << std::endl;
  Value *comp = Builder.CreateICmpNE(
      cond, ConstantInt::get(TheContext, APInt(1, 0, false)), "lazyandleft");
  std::cout << "over here2?" << std::endl;
    

  BasicBlock *rhs_ = BasicBlock::Create(TheContext, "andlhst", TheFunction);
  BasicBlock *true_ = BasicBlock::Create(TheContext, "andrhst");
  BasicBlock *end_ = BasicBlock::Create(TheContext, "andend");

  //if left true then jump to rhs_ branch and check if that is true as well
  // otherwise go straight to end
  Builder.CreateCondBr(comp, rhs_, end_);

  Builder.SetInsertPoint(rhs_);
  Value *right_cond = Casting(Type::getInt1Ty(TheContext) ,rhs->codegen(), tok, false);

  Value *right_comp = Builder.CreateICmpNE(
      right_cond, ConstantInt::get(TheContext, APInt(1, 0, false)), "lazyandright");
  //check if right is true and if so, then set truth to true or just end straight away
  Builder.CreateCondBr(right_comp, true_, end_);

  TheFunction->getBasicBlockList().push_back(true_);
  Builder.SetInsertPoint(true_);
  truth = ConstantInt::get(TheContext, APInt(1, 1, false));

  TheFunction->getBasicBlockList().push_back(end_);
  Builder.CreateBr(end_);
  Builder.SetInsertPoint(end_);

  return truth;
}




/**
 * Value*BinaryExprAST::codegen 
 * codegen for binary expressions
 */
Value *BinaryExprAST::codegen() {

  Value *left = LHS->codegen(); //convert LHS to value
  Value *right = RHS->codegen();  //convert RHS to value

  if (!left || !right) {  // if either lhs or rhs null then return null
    return nullptr;
  }
  switch (Op.type) {
  case (PLUS):

    // if either are floating point, we want to convert both to floating point
    if (left->getType()->isFloatTy() || right->getType()->isFloatTy()) {
      left = Casting(Type::getFloatTy(TheContext), left, Op, false);
      right = Casting(Type::getFloatTy(TheContext), right, Op, false);

      return Builder.CreateFAdd(left, right, "faddtmp");
    }
    // otherwise there are no floating points so we do integer add which casts
    // bools to ints
    return Builder.CreateAdd(left, right, "iaddtmp");

  case (MINUS):

    // if either are floating point, we want to convert both to floating point
    if (left->getType()->isFloatTy() || right->getType()->isFloatTy()) {
      left = Casting(Type::getFloatTy(TheContext), left, Op, false);
      right = Casting(Type::getFloatTy(TheContext), right, Op, false);
      return Builder.CreateFSub(left, right, "fsubtmp");
    }
    // otherwise there are no floating points so we do integer add which casts
    // bools to ints
    return Builder.CreateSub(left, right, "isubtmp");

  case (ASTERIX):

    if (left->getType()->isFloatTy() || right->getType()->isFloatTy()) {
      left = Casting(Type::getFloatTy(TheContext), left, Op, false);
      right = Casting(Type::getFloatTy(TheContext), right, Op, false);
      return Builder.CreateFMul(left, right, "fmultmp");
    }
    // otherwise there are no floating points so we do integer add which casts
    // bools to ints
    return Builder.CreateMul(left, right, "imultmp");

  case (DIV):

    if (left->getType()->isFloatTy() || right->getType()->isFloatTy()) {
      left = Casting(Type::getFloatTy(TheContext), left, Op, false);
      right = Casting(Type::getFloatTy(TheContext), right, Op, false);
      return Builder.CreateFDiv(left, right, "fdivtmp");
    }
    // otherwise there are no floating points so we do integer add which casts
    // bools to ints
    return Builder.CreateUDiv(left, right, "idivtmp");

  case (MOD):

    if (left->getType()->isFloatTy() || right->getType()->isFloatTy()) {
      left = Casting(Type::getFloatTy(TheContext), left, Op, false);
      right = Casting(Type::getFloatTy(TheContext), right, Op, false);
      return Builder.CreateFRem(left, right, "fmodtmp");
    }
    // otherwise there are no floating points so we do integer add which casts
    // bools to ints
    return Builder.CreateURem(left, right, "imodtmp");

  case (EQ):

    left = Casting(Type::getFloatTy(TheContext), left, Op, false);
    right = Casting(Type::getFloatTy(TheContext), right, Op, false);
    return Builder.CreateFCmpOEQ(left, right, "feqtmp");

  case (NE):

    left = Casting(Type::getFloatTy(TheContext), left, Op, false);
    right = Casting(Type::getFloatTy(TheContext), right, Op, false);
    return Builder.CreateFCmpONE(left, right, "fneqtmp");

  case (LE):

    left = Casting(Type::getFloatTy(TheContext), left, Op, false);
    right = Casting(Type::getFloatTy(TheContext), right, Op, false);
    return Builder.CreateFCmpOLE(left, right, "fletmp");

  case (LT):

    left = Casting(Type::getFloatTy(TheContext), left, Op, false);
    right = Casting(Type::getFloatTy(TheContext), right, Op, false);
    return Builder.CreateFCmpOLT(left, right, "flttmp");

  case (GE):

    left = Casting(Type::getFloatTy(TheContext), left, Op, false);
    right = Casting(Type::getFloatTy(TheContext), right, Op, false);
    return Builder.CreateFCmpOGE(left, right, "fgetmp");

  case (GT):

    left = Casting(Type::getFloatTy(TheContext), left, Op, false);
    right = Casting(Type::getFloatTy(TheContext), right, Op, false);
    return Builder.CreateFCmpOGT(left, right, "ffttmp");

  case (OR): {
    std::cout << "here or" << std::endl;

    TOKEN o = Op;
    return LazyOr(std::move(LHS), std::move(RHS), o);
    // left = Casting(Type::getInt1Ty(TheContext), left, Op, false);
    // right = Casting(Type::getInt1Ty(TheContext), right, Op, false);
    // return Builder.CreateOr(left, right, "ortmp");
  }

  case (AND): {
    std::cout << "here and" << std::endl;
    TOKEN o = Op;
    return LazyAnd(std::move(LHS), std::move(RHS), o);
    // left = Casting(Type::getInt1Ty(TheContext), left, Op, false);
    // right = Casting(Type::getInt1Ty(TheContext), right, Op, false);
    // return Builder.CreateAnd(left, right, "andtmp");
  }

  }
  TOKEN t = Op;
  LogSemanticError(t, "invalid binary operator");
  return nullptr;
}
/**
 * Value*UnaryExprAST::codegen 
 * codegen for unary expressions
 */
Value *UnaryExprAST::codegen() {
  Value *V = Expr->codegen(); //convert expression into value

  if (!V) 
    return nullptr;

  switch (Op.type) {
  case (NOT): //! 

    V = Casting(Type::getInt1Ty(TheContext), V, Op, false);
    return Builder.CreateNot(V, "nottmp");

  case (MINUS)://-

  //if integer value, then use CreateNeg otherwise use FNeg
    if (V->getType()->isIntegerTy(1) || V->getType()->isIntegerTy(32)) {  
      V = Casting(Type::getInt32Ty(TheContext), V, Op, false);
      return Builder.CreateNeg(V, "inegtmp");
    }
    V = Casting(Type::getFloatTy(TheContext), V, Op, false);
    return Builder.CreateFNeg(V, "fnegtmp");

  }
  TOKEN t = Op;
  LogSemanticError(t, "invalid unary operator");
  return nullptr;
}
/**
 * Value*CallExprAST::codegen 
 * code gen for function calls
 */
Value *CallExprAST::codegen() {
  Function *CalleeF = TheModule->getFunction(Callee.lexeme);

  // no such function exist
  if (!CalleeF) {
    TOKEN t = Callee;
    LogSemanticError(t, "undefined reference to \"" + t.lexeme + "\"");
    return nullptr;
  }

  //if argument size does not match callee arg size
  if (CalleeF->arg_size() < Args.size()) {
    TOKEN t = Callee;
    std::string message = "too many arguments to function call, expected " +
                          std::to_string(CalleeF->arg_size()) + " but got " +
                          std::to_string(Args.size());
    LogSemanticError(t, message);
  } else if (CalleeF->arg_size() > Args.size()) {
    TOKEN t = Callee;
    std::string message = "too few arguments to function call, expected " +
                          std::to_string(CalleeF->arg_size()) + " but got " +
                          std::to_string(Args.size());
    LogSemanticError(t, message);
  }

  std::vector<Value *> ArgsV;

  std::vector<Type *> ParameterTypes;

  //push back arguments into callee parameter with correct type
  for (auto &Arg : CalleeF->args()) {
    ParameterTypes.push_back(Arg.getType());
  }
  for (int i = 0; i < Args.size(); i++) {
    ArgsV.push_back(Casting(ParameterTypes[i], Args[i]->codegen(), Callee, true));
  }

  return Builder.CreateCall(CalleeF, ArgsV, "calltmp");
}
/**
 * llvm::Type*getType 
 * Given token, get respective Type* 
 * @param  {TOKEN} t : token to get type from
 */
llvm::Type *getType(TOKEN t) {
  switch (t.type) {
  case (INT_TOK):
    return Type::getInt32Ty(TheContext);
  case (FLOAT_TOK):
    return Type::getFloatTy(TheContext);
  case (BOOL_TOK):
    return Type::getInt1Ty(TheContext);
  case (VOID_TOK):
    return Type::getVoidTy(TheContext);
  }

  return nullptr;
}

/**
 * Function*PrototypeAST::codegen 
 * codegen for prototype
 * 
 */
Function *PrototypeAST::codegen() {

  std::vector<llvm::Type *> Arguments;

  // load correct types into prototype arguments list
  for (int i = 0; i < Args.size(); i++) {
    if (Args[i] && Args[i]->Type.lexeme != "void")  //if void type, treat as if no arguments
      Arguments.push_back(getType(Args[i]->Type));
  }

  FunctionType *FT = FunctionType::get(getType(Type), Arguments, false);

  Function *F = Function::Create(FT, Function::ExternalLinkage, Name.lexeme,
                                 TheModule.get());

  // set names for all arguments
  int i = 0;
  for (auto &Arg : F->args()) {
    Arg.setName(Args.at(i++)->Name.lexeme);
  }

  return F;
}

/**
 * Function*FunctionAST::codegen 
 * codegen for function declaration
 */
Function *FunctionAST::codegen() {
  Scopes.push_back(std::map<std::string, AllocaInst *>());  //push new scope for parameters
  Function *TheFunction = TheModule->getFunction(Proto->getName()); //check for existing function from previous extern 

  if (!TheFunction)
    TheFunction = Proto->codegen();

  if (!TheFunction)
    return nullptr;

  //create block and set insert point
  BasicBlock *BB = BasicBlock::Create(TheContext, "entry", TheFunction);
  Builder.SetInsertPoint(BB);

  // make alloca for each parameter
  for (auto &Arg : TheFunction->args()) {

    AllocaInst *Alloca = CreateEntryBlockAlloca(
        TheFunction, std::string(Arg.getName()), Arg.getType());
    Builder.CreateStore(&Arg, Alloca);

    Scopes.back()[std::string(Arg.getName())] = Alloca; //for parameter name, assign alloca in current scope 
  }

  Body->codegen();  

  verifyFunction(*TheFunction); //verify function

  Scopes.pop_back();  //pop scope

  return TheFunction;
}


/**
 * * Value*IfAST::codegen 
 * codegen for if statements
 */
Value *IfAST::codegen() {
  Scopes.push_back(std::map<std::string, AllocaInst *>());
  Function *TheFunction = Builder.GetInsertBlock()->getParent();

  // generate condition
  Value *cond = Condition->codegen();

  // convert condition to bool by doing != 0
  Value *comp = Builder.CreateICmpNE(
      cond, ConstantInt::get(TheContext, APInt(1, 0, false)), "ifcond");
  // create blocks
  BasicBlock *true_ = BasicBlock::Create(TheContext, "then", TheFunction);

  BasicBlock *false_ = BasicBlock::Create(TheContext, "else");

  BasicBlock *end_ = BasicBlock::Create(TheContext, "end");
  // check if else is empty and if so, create condition branch with only true_
  // and end_

  if (!ElseBody) {
    Builder.CreateCondBr(comp, true_, end_);
  } else {
    Builder.CreateCondBr(comp, true_, false_);
  }
  // if true
  Builder.SetInsertPoint(true_);
  IfBody->codegen();
  Builder.CreateBr(end_);

  // if false
  TheFunction->getBasicBlockList().push_back(false_);
  Builder.SetInsertPoint(false_);
  if (ElseBody)
    ElseBody->codegen();

  // end
  TheFunction->getBasicBlockList().push_back(end_);
  Builder.CreateBr(end_);
  Builder.SetInsertPoint(end_);

  Scopes.pop_back();
  return nullptr;
}
/**
 * Value*WhileAST::codegen 
 * codegen for while loops
 */
Value *WhileAST::codegen() {
  Scopes.push_back(std::map<std::string, AllocaInst *>());

  Function *TheFunction = Builder.GetInsertBlock()->getParent();
  // condition block
  BasicBlock *before = BasicBlock::Create(TheContext, "before", TheFunction);

  // loop block
  BasicBlock *loop = BasicBlock::Create(TheContext, "loop");

  // block afer loop
  BasicBlock *exit = BasicBlock::Create(TheContext, "end");

  Builder.CreateBr(before);
  Builder.SetInsertPoint(before);
  // generate condition and generate branch which decides to loop or exit
  // based off of condtion
  Value *cond = Condition->codegen();

  Value *comp = Builder.CreateICmpNE(
      cond, ConstantInt::get(TheContext, APInt(1, 0, false)), "whilecond");

  TheFunction->getBasicBlockList().push_back(loop);

  Builder.CreateCondBr(comp, loop, exit);

  Builder.SetInsertPoint(loop);

  // work inside body

  Body->codegen();

  Builder.CreateBr(before);

  // exit branch
  TheFunction->getBasicBlockList().push_back(exit);
  Builder.CreateBr(exit);
  Builder.SetInsertPoint(exit);

  Scopes.pop_back();
  return nullptr;
}
/**
 * Value*ReturnAST::codegen 
 * codegen for return 
 */
Value *ReturnAST::codegen() {
  llvm::Type *FuncReturnType = Builder.getCurrentFunctionReturnType();

  if (Body) {
    // grab value from return body codegen
    Value *V = Body->codegen();
    return_flag = 1;  // we have found a return 

    if (V->getType() != FuncReturnType) { //incorrect return type for function 
      TOKEN tok = Tok;
      LogSemanticError(tok, "return type different to prototype type");
    }
    return Builder.CreateRet(V);
  } else {  // return with no body so void
    return_flag = 1;   //return found
    return Builder.CreateRetVoid();
  }
  return nullptr;
}
/**
 * Value*VarDeclAST::codegen 
 * codegen for variable declarations
 */
Value *VarDeclAST::codegen() {
  // if scope empty that means it is a global variable
  if (Scopes.empty()) {
    if (GlobalVariables.count(Name.lexeme) >
        0) { // error redefinition of global variable
      TOKEN t = Name;
      LogSemanticError(t, "redefinition of variable \"" + t.lexeme + "\"");
    }

    GlobalVariable *g = new GlobalVariable(
        *(TheModule.get()), getType(Type), false, GlobalValue::CommonLinkage,
        Constant::getNullValue(getType(Type)));

    GlobalVariables[Name.lexeme] = g;
    return nullptr;
  }

  // Local declarations
  Function *TheFunction = Builder.GetInsertBlock()->getParent();
  //create alloca entry for variable 
  AllocaInst *Alloca =
      CreateEntryBlockAlloca(TheFunction, Name.lexeme, getType(Type));

  Builder.CreateStore(Constant::getNullValue(getType(Type)), Alloca);
  //assign to current scope
  Scopes.back()[Name.lexeme] = Alloca;
  return nullptr;
}
/**
 * Value*VarAssignAST::codegen 
 * code gen for variable assign
 */
Value *VarAssignAST::codegen() {
  Value *Val = Expr->codegen(); //convert expression into value

  int found_flag = 0; // check if variable has been found

  if (!Val) {

    return nullptr;
  }

  // want to reflect change for all scopes
  for (int i = Scopes.size() - 1; i >= 0; i--) {
    if (Scopes[i].count(Name.lexeme) > 0) {
      Function *TheFunction = Builder.GetInsertBlock()->getParent();
      AllocaInst *Alloca = Scopes[i][Name.lexeme];
      Value *CastedVal = Casting(Alloca->getAllocatedType(), Val, Name, false);
      Builder.CreateStore(CastedVal, Alloca);
      Scopes[i][Name.lexeme] = Alloca;

      found_flag = 1;

      // we break here as the variable has just been declared in this current
      // scope
      if (i == Scopes.size() - 1)
        break;
    }
  }

  if (GlobalVariables.count(Name.lexeme) > 0) {

    GlobalVariable *gAlloca = GlobalVariables.at(Name.lexeme);

    Value *CastedVal = Casting(gAlloca->getValueType(), Val, Name, false);

    Builder.CreateStore(CastedVal, gAlloca);

    GlobalVariables[Name.lexeme] = gAlloca;

    found_flag = 1;
  }
  // error, assigning value to undeclared variable.
  if (found_flag != 1) {
    TOKEN t = Name;
    LogSemanticError(t, "use of undeclared identifier \"" + t.lexeme + "\"");
    return nullptr;
  }

  //here we should not return null ptr as another assign may use this value 
  // i.e. for cases such as x = y = 1
  return Val;
}
