#include "codegen.hpp"

LLVMContext TheContext;
IRBuilder<> Builder(TheContext);
std::unique_ptr<Module> TheModule;

AllocaInst* CreateEntryBlockAlloca(Function *TheFunction, const std::string &VarName, llvm::Type* type) {
  IRBuilder<> TmpB(&TheFunction->getEntryBlock(), TheFunction->getEntryBlock().begin());

  return TmpB.CreateAlloca(type, 0, VarName.c_str());
}

std::vector<std::map<std::string, AllocaInst*>> Scopes; 
std::map<std::string, GlobalVariable*> GlobalValues;



Value *ProgramAST::codegen() {
  for (int i = 0; i < ExternList.size(); i++) {
    if (ExternList[i])
      ExternList[i]->codegen();
  }
  
  
  for (int i = 0; i < DeclList.size(); i++) {
    if (DeclList[i])
      DeclList[i]->codegen();
  }


  return nullptr;
}

Value *BlockAST::codegen() {
  Scopes.push_back(std::map<std::string, AllocaInst*>());
  for (int i = 0; i < LocalDecls.size(); i++) {
    if (LocalDecls[i])
      LocalDecls[i]->codegen();
  }

  // for (int i = 0; i < StmtList.size(); i++) {
  //   if (StmtList[i])
  //     StmtList[i]->codegen();
  // }
  Scopes.pop_back();
  return nullptr;
}

Value *LiteralASTNode::codegen() {
  switch (Tok.type) {
  case (INT_TOK):
    std::cout << Tok.lexeme << std::endl;
    return ConstantInt::get(TheContext, APInt(std::stoi(Tok.lexeme), false));
  case (FLOAT_TOK):
    return ConstantFP::get(TheContext, APFloat(std::stof(Tok.lexeme)));

  case (BOOL_TOK):

    return ConstantInt::get(TheContext, APInt(std::stoi(Tok.lexeme), false));
  case (IDENT):
    std::string Name = Tok.lexeme;
    AllocaInst *V = Scopes.back()[Name];

    if (!V)
      return nullptr;

    return Builder.CreateLoad(V->getAllocatedType(), V, Name.c_str());
    
  }

  return nullptr;
}


Value *BinaryExprAST::codegen() {

  // switch (Op.type) {
  // case (PLUS):
    
  //   return Builder.CreateAdd(LHS->codegen(), RHS->codegen(), "addtmp");
  // case (MINUS):
  //   return Builder.CreateSub(LHS->codegen(), RHS->codegen(), "subtmp");
  // case (ASTERIX):
  //   return Builder.CreateMul(LHS->codegen(), RHS->codegen(), "multmp");
  // case (DIV):
  //   return Builder.CreateSDiv(LHS->codegen(), RHS->codegen(), "divtmp");
  // case (MOD):
  //   return Builder.CreateSRem(LHS->codegen(), RHS->codegen(), "modtmp");
  // case (EQ):
  //   return Builder.CreateICmpEQ(LHS->codegen(), RHS->codegen(), "eqtmp");
  // case (NE):
  //   return Builder.CreateICmpNE(LHS->codegen(), RHS->codegen(), "netmp");
  // case (LE):
  //   return Builder.CreateICmpULE(LHS->codegen(), RHS->codegen(), "uletmp");
  // case (LT):
  //   return Builder.CreateICmpULT(LHS->codegen(), RHS->codegen(), "ulttmp");
  // case (GE):
  //   return Builder.CreateICmpUGE(LHS->codegen(), RHS->codegen(), "ugetmp");
  // case (GT):
  //   return Builder.CreateICmpUGT(LHS->codegen(), RHS->codegen(), "ugttmp");
  // }

  // return nullptr;
}


Value *UnaryExprAST::codegen() {
  // switch (Op.type) {
  // case (NOT):
  //   return Builder.CreateNot(Expr->codegen(), "nottmp");
  // case (MINUS):
  //   return Builder.CreateNeg(Expr->codegen(), "negtmp");
  // }

  // return nullptr;
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

// code generation for the prototype of a function 
Function *PrototypeAST::codegen() {
 
  std::vector<llvm::Type*> Arguments;

  //load correct types into prototype arguments list
  for (int i = 0; i < Args.size(); i++) {
    if (Args[i])
      Arguments.push_back(getType(Args[i]->Type));
  }

  FunctionType *FT =
  FunctionType::get(getType(Type), Arguments, false);

  Function *F =
      Function::Create(FT, Function::ExternalLinkage, Name.lexeme, TheModule.get());

  //set names for all arguments
  int i = 0;
  for (auto &Arg : F->args()) {
    Arg.setName(Args.at(i++)->Name.lexeme);
  }

  return F;
}


Function *FunctionAST::codegen() {
  Scopes.push_back(std::map<std::string, AllocaInst*>());
  Function *TheFunction = TheModule->getFunction(Proto->getName());

  if (!TheFunction)
    TheFunction = Proto->codegen();

  if (!TheFunction)
    return nullptr;

  BasicBlock *BB = BasicBlock::Create(TheContext, "entry", TheFunction);
  Builder.SetInsertPoint(BB);

  //make alloca for each parameter
  for (auto &Arg: TheFunction->args()) {

    AllocaInst *Alloca = CreateEntryBlockAlloca(TheFunction, std::string(Arg.getName()), getType(Proto->getProtoType()));
    Builder.CreateStore(&Arg, Alloca);
    Scopes.back()[std::string(Arg.getName())] = Alloca;
  }

  Value *RetVal = Body->codegen();

  if (RetVal) {
    Builder.CreateRet(RetVal);
    verifyFunction(*TheFunction);
    return TheFunction;

  }
  return TheFunction;
  TheFunction->eraseFromParent();
  Scopes.pop_back();
  return nullptr;

}

// code generation for IfElse/If statments 
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

  // check if end is empty and if so, create condition branch with only true_ and end_
  if (!end_) {
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
  ElseBody->codegen();

  TheFunction->getBasicBlockList().push_back(end_);
  Builder.CreateBr(end_);
  Builder.SetInsertPoint(end_);

  return nullptr;
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

  Builder.SetInsertPoint(loop);

  // leave loop
  TheFunction->getBasicBlockList().push_back(exit);
  Builder.CreateBr(exit);
  Builder.SetInsertPoint(exit);

  return nullptr;
}


Value *ReturnAST::codegen() { 
  if (Body) {
    //grab value from return body codegen
    Value *V = Body->codegen();

    return Builder.CreateRet(V);
  } else {
    return Builder.CreateRetVoid();
  }
  return nullptr; 
}


Value *VarDeclAST::codegen() { 
  //Global variables
  if (Scopes.empty()) {
    GlobalVariable* g = new GlobalVariable(
      *(TheModule.get()),
      getType(Type),
      false,
      GlobalValue::CommonLinkage,
      Constant::getNullValue(getType(Type))
    );

    GlobalValues[Name.lexeme] = g;
    return nullptr;
  }

  // Local declarations
  Function *TheFunction = Builder.GetInsertBlock()->getParent();

  AllocaInst* Alloca =
    CreateEntryBlockAlloca(TheFunction, Name.lexeme, getType(Type));
  
  Builder.CreateStore(Constant::getNullValue(getType(Type)), Alloca);

  Scopes.back()[Name.lexeme] = Alloca;
  return nullptr;
}


Value *VarAssignAST::codegen() {

  // Value *Val = Expr->codegen();

  // if (!Val)
  //   return nullptr;

  // Value *Variable = NamedValues[Name.lexeme];

  // if (!Variable)
  //   return nullptr;

  // Builder.CreateStore(Val, Variable);
  
  // return Val;
}

