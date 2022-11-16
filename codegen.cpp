#include "codegen.hpp"
#include <llvm/IR/GlobalVariable.h>

using namespace llvm;
using namespace llvm::sys;


LLVMContext TheContext;
IRBuilder<> Builder(TheContext);
std::unique_ptr<Module> TheModule;

AllocaInst* CreateEntryBlockAlloca(Function *TheFunction, const std::string &VarName, llvm::Type* type) {
  IRBuilder<> TmpB(&TheFunction->getEntryBlock(), TheFunction->getEntryBlock().begin());

  return TmpB.CreateAlloca(type, 0, VarName.c_str());
}

Value *Casting(Type *VarType, Value *NewVal) {
  //create expression into 32 bit integer
  if (VarType->isIntegerTy(32)) {
    if (NewVal->getType()->isIntegerTy(32)) {
      return NewVal;
    } else if (NewVal->getType()->isIntegerTy(1)) {
      return Builder.CreateIntCast(NewVal, Type::getInt32Ty(TheContext), false, "btoi32");
    } else if (NewVal->getType()->isFloatTy()) {
      return Builder.CreateFPToUI(NewVal, Type::getInt32Ty(TheContext), "ftoi32");
    } 
  //convert expression into boolean
  } else if (VarType->isIntegerTy(1)) {
    if (NewVal->getType()->isIntegerTy(32)) {
      return Builder.CreateBitCast(NewVal, Type::getInt1Ty(TheContext), "i32tob");
    } else if (NewVal->getType()->isIntegerTy(1)) {
      return NewVal;
    } else if (NewVal->getType()->isFloatTy()) {
      return Builder.CreateFPToUI(NewVal, Type::getInt1Ty(TheContext), "ftob");
    }
  //convert expression int floating point
  } else if (VarType->isFloatTy()) {
    if (NewVal->getType()->isIntegerTy(32)) {
      return Builder.CreateUIToFP(NewVal, Type::getFloatTy(TheContext), "i32tof");
    } else if (NewVal->getType()->isIntegerTy(1)) {
      return Builder.CreateUIToFP(NewVal, Type::getFloatTy(TheContext), "i1tof");
    } else if (NewVal->getType()->isFloatTy()) {
      return NewVal;
    }
  }

  return nullptr;
}

std::vector<std::map<std::string, AllocaInst*>> Scopes; 
std::map<std::string, GlobalVariable*> GlobalVariables;



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

  for (int i = 0; i < StmtList.size(); i++) {
    if (StmtList[i])
      StmtList[i]->codegen();
  }
  Scopes.pop_back();
  return nullptr;
}

Value *LiteralASTNode::codegen() {
  switch (Tok.type) {
  case (INT_LIT):
    return ConstantInt::get(TheContext, APInt(32, std::stoi(Tok.lexeme), false));
  case (FLOAT_LIT):
    return ConstantFP::get(TheContext, APFloat(std::stof(Tok.lexeme)));

  case (BOOL_LIT): {

    int bool_ = (Tok.lexeme == "true") ? 1 : 0;
    return ConstantInt::get(TheContext, APInt(1, bool_, false));
  }
  case (IDENT):
    
    AllocaInst *V; 

    // also do global case
    for (int i = Scopes.size() - 1; i >= 0; i--) {
      if (Scopes[i].count(Tok.lexeme) > 0) {
        V = Scopes[i][Tok.lexeme];
        break;
      } 
    }

    if (!V) {

      GlobalVariable *G = GlobalVariables[Tok.lexeme];
      if (!G)
        return nullptr;
      return Builder.CreateLoad(G->getValueType(), G, Tok.lexeme);
    } else { 
      return Builder.CreateLoad(V->getAllocatedType(), V, Tok.lexeme);
    }
    
    
  }

  return nullptr;
}


Value *BinaryExprAST::codegen() {


  Value *left = LHS->codegen();
  Value *right = RHS->codegen();



  if (!left || !right ) {
    return nullptr;
  }
  switch (Op.type) {
  case (PLUS):
    // if either are floating point, we want to convert both to floating point
    if (left->getType()->isFloatTy() || right->getType()->isFloatTy()) {
      left = Casting(Type::getFloatTy(TheContext), left);
      right = Casting(Type::getFloatTy(TheContext), right);
      return Builder.CreateFAdd(left, right, "faddtmp");
    } 
    // otherwise there are no floating points so we do integer add which casts bools to ints
    return Builder.CreateAdd(left, right,"iaddtmp");
    
  case (MINUS):
    // if either are floating point, we want to convert both to floating point
    if (left->getType()->isFloatTy() || right->getType()->isFloatTy()) {
      left = Casting(Type::getFloatTy(TheContext), left);
      right = Casting(Type::getFloatTy(TheContext), right);
      return Builder.CreateFSub(left, right, "fsubtmp");
    } 
    // otherwise there are no floating points so we do integer add which casts bools to ints
    return Builder.CreateSub(left, right,"isubtmp");
  case (ASTERIX):
    if (left->getType()->isFloatTy() || right->getType()->isFloatTy()) {
      left = Casting(Type::getFloatTy(TheContext), left);
      right = Casting(Type::getFloatTy(TheContext), right);
      return Builder.CreateFMul(left, right, "fmultmp");
    } 
    // otherwise there are no floating points so we do integer add which casts bools to ints
    return Builder.CreateMul(left, right,"imultmp");
  case (DIV):
    if (left->getType()->isFloatTy() || right->getType()->isFloatTy()) {
      left = Casting(Type::getFloatTy(TheContext), left);
      right = Casting(Type::getFloatTy(TheContext), right);
      return Builder.CreateFDiv(left, right, "fdivtmp");
    } 
    // otherwise there are no floating points so we do integer add which casts bools to ints
    return Builder.CreateUDiv(left, right,"idivtmp");
  case (MOD):
    if (left->getType()->isFloatTy() || right->getType()->isFloatTy()) {
      left = Casting(Type::getFloatTy(TheContext), left);
      right = Casting(Type::getFloatTy(TheContext), right);
      return Builder.CreateFRem(left, right, "fmodtmp");
    } 
    // otherwise there are no floating points so we do integer add which casts bools to ints
    return Builder.CreateURem(left, right, "imodtmp");
  case (EQ):
      left = Casting(Type::getFloatTy(TheContext), left);
      right = Casting(Type::getFloatTy(TheContext), right);
      return Builder.CreateFCmpOEQ(left, right, "feqtmp");
    return Builder.CreateICmpEQ(left, right, "ieqtmp");
  case (NE):
      left = Casting(Type::getFloatTy(TheContext), left);
      right = Casting(Type::getFloatTy(TheContext), right);
      return Builder.CreateFCmpONE(left, right, "fneqtmp");
  case (LE):
      left = Casting(Type::getFloatTy(TheContext), left);
      right = Casting(Type::getFloatTy(TheContext), right);
      return Builder.CreateFCmpOLE(left, right, "fletmp");
  case (LT):
      left = Casting(Type::getFloatTy(TheContext), left);
      right = Casting(Type::getFloatTy(TheContext), right);
      return Builder.CreateFCmpOLT(left, right, "flttmp");
  case (GE):
      left = Casting(Type::getFloatTy(TheContext), left);
      right = Casting(Type::getFloatTy(TheContext), right);
      return Builder.CreateFCmpOGE(left, right, "fgetmp");
  case (GT):
      left = Casting(Type::getFloatTy(TheContext), left);
      right = Casting(Type::getFloatTy(TheContext), right);
      return Builder.CreateFCmpOGT(left, right, "ffttmp");
  }
  return nullptr;
}


Value *UnaryExprAST::codegen() {
  Value *V = Expr->codegen();

  if (!V)
    return nullptr;

  
  switch (Op.type) {
  case (NOT):
    return Builder.CreateNot(V, "nottmp");
  case (MINUS):
    V = Casting(Type::getFloatTy(TheContext), V);
    return Builder.CreateFNeg(V, "negtmp");
  }

  return nullptr;
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
      std::cout << "at void here" << std::endl;
      return Type::getVoidTy(TheContext);
  }

  return nullptr;
}

// code generation for the prototype of a function 
Function *PrototypeAST::codegen() {
 
  std::vector<llvm::Type*> Arguments;


  
  //load correct types into prototype arguments list
  for (int i = 0; i < Args.size(); i++) {
    if (Args[i] && Args[i]->Type.lexeme != "void")
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


  std::cout << "can i make here" << std::endl;

  return nullptr;

}

// code generation for IfElse/If statments 
Value *IfAST::codegen() {
  Function *TheFunction = Builder.GetInsertBlock()->getParent();
  
  // generate condition
  Value *cond = Condition->codegen();
  // convert condition to bool by comparing != 0
  Value *comp = Builder.CreateICmpNE(
      cond, ConstantInt::get(TheContext, APInt(1, 0, false)), "ifcond");
  // create blocks
  BasicBlock *true_ = BasicBlock::Create(TheContext, "then", TheFunction);

  BasicBlock *false_ = BasicBlock::Create(TheContext, "else");

  BasicBlock *end_ = BasicBlock::Create(TheContext, "end");
  Scopes.push_back(std::map<std::string, AllocaInst*>());
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

  Scopes.pop_back();
  return nullptr;
}


Value *WhileAST::codegen() {
  Scopes.push_back(std::map<std::string, AllocaInst*>());

  Function *TheFunction = Builder.GetInsertBlock()->getParent();
  //condition block
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


Value *ReturnAST::codegen() { 
  if (Body) {
    //grab value from return body codegen
    Value *V = Body->codegen();
    // need some code to compare type of body to type of function 
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

    GlobalVariables[Name.lexeme] = g;
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
  Value *Val = Expr->codegen();

  if (!Val)
    return nullptr;

  

  for (int i = Scopes.size() - 1; i >= 0; i--) {
    if (Scopes[i].count(Name.lexeme) > 0) {
      Function *TheFunction = Builder.GetInsertBlock()->getParent();
      AllocaInst* Alloca = Scopes[i][Name.lexeme];
      Value *CastedVal = Casting(Alloca->getAllocatedType(), Val);
      Builder.CreateStore(CastedVal, Alloca);
      Scopes[i][Name.lexeme] = Alloca;
    } 
  }

  if (GlobalVariables.count(Name.lexeme) > 0) {
    
    GlobalVariable* gAlloca = GlobalVariables.at(Name.lexeme);

    Value *CastedVal = Casting(gAlloca->getValueType(),  Val);

    Builder.CreateStore(CastedVal, gAlloca);

    GlobalVariables[Name.lexeme] = gAlloca;
  }

  
  return nullptr;
}

