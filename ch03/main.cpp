//
// Created by 吴凡 on 2017/5/10.
//

#include "llvm/ADT/APFloat.h"
#include "llvm/ADT/STLExtras.h"
#include "llvm/IR/BasicBlock.h"
#include "llvm/IR/Constants.h"
#include "llvm/IR/DerivedTypes.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Type.h"
#include "llvm/IR/Verifier.h"

#include <string>
#include <vector>
#include <map>
#include <memory>
#include <algorithm>

/*
 * def fib(x)
 *    if x < 3 then
 *      1
 *    else
 *      fib(x-1) + fib(x-2)
 *
 * fib(40)
 *
 * extern sin(arg);
 * extern cos(arg);
 * extern atan2(arg1 arg2);
 * atan2(sin(.4), cos(42))
 */

enum Token {
  tok_eof = -1,

  // commands

  tok_def = -2,
  tok_extern = -3,

  // primary

  tok_identifier = -4,
  tok_number = -5,
};

static double numVal;
static std::string identifierStr;

static int gettok() {
  static int lastChar = ' ';// getchar();

  while (isspace(lastChar)) {
    lastChar = getchar();
  }

  // identifier ::= [a-zA-Z][0-9a-zA-Z]*
  if (isalpha(lastChar)) {
    identifierStr = lastChar;
    while (isalnum(lastChar = getchar())) {
      identifierStr += lastChar;
    }

    if (identifierStr == "def") {
      return tok_def;
    } else if (identifierStr == "extern") {
      return tok_extern;
    } else {
      return tok_identifier;
    }
  }

  // number ::= [0-9.]+
  if (isnumber(lastChar) || lastChar == '.') {
    std::string numStr;
    do {
      numStr += lastChar;
      lastChar = getchar();
    } while (isnumber(lastChar) || lastChar == '.');

    numVal = strtod(numStr.c_str(), nullptr);
    return tok_number;
  }

  // comment
  if (lastChar == '#') {
    do {
      lastChar = getchar();
    } while (lastChar != EOF || lastChar != '\n' || lastChar != '\r');

    if (lastChar != EOF) {
      return gettok();
    }
  }

  if (lastChar == EOF) {
    return tok_eof;
  }

  int thisChar = lastChar;
  lastChar = getchar();
  return thisChar;
}

namespace {

/// ExprAST - Base class for all expression nodes.
class ExprAST {
public:
  virtual ~ExprAST() = default;
  virtual llvm::Value *codegen() = 0;
};

/// NumberExprAST - Expression class for numeric literals like "1.0".
class NumberExprAST : public ExprAST {
  double val;
public:
  NumberExprAST(double val) : val(val) {}

  llvm::Value *codegen() override;
};

/// VariableExprAST - Expression class for referencing a variable, like "a".
class VariableExprAST : public ExprAST {
  std::string name;
public:
  VariableExprAST(const std::string &name) : name(name) {}
  llvm::Value *codegen() override;
};

/// BinaryExprAST - Expression class for a binary operator
class BinaryExprAST : public ExprAST {
  char op;
  std::unique_ptr<ExprAST> lhs, rhs;
public:
  BinaryExprAST(char op, std::unique_ptr<ExprAST> lhs, std::unique_ptr<ExprAST> rhs) :
      op(op), lhs(std::move(lhs)), rhs(std::move(rhs)) {}
  llvm::Value *codegen() override;
};

/// CallExprAST - Expression class for function calls
class CallExprAST : public ExprAST {
  std::string callee;
  std::vector<std::unique_ptr<ExprAST>> args;
public:
  CallExprAST(const std::string &callee, std::vector<std::unique_ptr<ExprAST>> args) :
      callee(callee), args(std::move(args)) {}
  llvm::Value *codegen() override;
};

/// PrototypeAST - This class represents the "prototype" for a function,
/// which captures its name, and its argument names (thus implicitly the number
/// of arguments the function takes).
class PrototypeAST {
  std::string name;
  std::vector<std::string> args;

public:
  PrototypeAST(const std::string &name, std::vector<std::string> args) :
      name(name), args(std::move(args)) {}
  llvm::Function *codegen();
  const std::string &getName() const {
    return name;
  }
};

/// FunctionAST - This class represents a function definition itself.
class FunctionAST {
  std::unique_ptr<PrototypeAST> proto;
  std::unique_ptr<ExprAST> body;

public:
  FunctionAST(std::unique_ptr<PrototypeAST> proto, std::unique_ptr<ExprAST> body) :
      proto(std::move(proto)), body(std::move(body)) {}
  llvm::Function *codegen();
};

} // end anonymous namespace

//===--------------------------------------------------------------------------------===//
// Abstract Syntax Tree (aka Parse Tree)
//===--------------------------------------------------------------------------------===//

/*
 * x+y
 * auto lhs = llvm::make_unique<VariableExprAST>("x");
 * auto rhs = llvm::make_unique<VariableExprAST>("y");
 * auto result = llvm::make_unique<BinaryExprAST>('+', std::move(lhs), std::move(rhs));
 */

/// CurTok/getNextToken - Provide a simple token buffer. curTok is the current
/// token the parser is looking at. getNextToken reads another token from the
/// lexer and updates curTok with its result.
static int curTok;
static int getNextToken() {
  return curTok = gettok();
}

std::unique_ptr<ExprAST> logError(const char *str) {
  fprintf(stderr, "LogError: %s\n", str);
  return nullptr;
}

std::unique_ptr<PrototypeAST> logErrorP(const char *str) {
  logError(str);
  return nullptr;
}

static std::unique_ptr<ExprAST> parseExpression();

/// numberexpr ::= number
static std::unique_ptr<ExprAST> parseNumberExpr() {
  auto result = llvm::make_unique<NumberExprAST>(numVal);
  getNextToken();
  return std::move(result);
}

/// identifierexpr
///   ::= identifier
///   ::= identifier '(' expression* ')'
static std::unique_ptr<ExprAST> parseIdentifierExpr() {
  std::string idName = identifierStr;
  getNextToken(); // eat identifier

  if (curTok != '(')
    return llvm::make_unique<VariableExprAST>(idName);

  // Call.
  getNextToken(); // eat '('
  std::vector<std::unique_ptr<ExprAST>> args;
  if (curTok != ')') {
    while (1) {
      if (auto arg = parseExpression())
        args.push_back(std::move(arg));
      else
        return nullptr;
      if (curTok == ')')
        break;
      if (curTok != ',')
        return logError("Expected ')' or ',' in argument list");
      getNextToken();
    }
  }
  // eat the ')'
  getNextToken();
  return llvm::make_unique<CallExprAST>(idName, std::move(args));
}

/// parenexpr ::= '(' expression ')'
static std::unique_ptr<ExprAST> parseParenExpr() {
  getNextToken(); // eat '('
  auto v = parseExpression();
  if (!v)
    return nullptr;
  if (curTok != ')')
    return logError("expected ')'");
  getNextToken(); // eat ')'
  return v;
}

/// primary
///   ::= identifier
///   ::= numberexpr
///   ::= parenexpr
static std::unique_ptr<ExprAST> parsePrimary() {
  switch(curTok) {
  case tok_identifier:
    return parseIdentifierExpr();
  case tok_number:
    return parseNumberExpr();
  case '(':
    return parseParenExpr();
  default:
    return logError("unknown token when expecting an expression");
  }
}

static std::map<char, int> binopPrecedence;

static int getTokPrecedence() {
  if(!isascii(curTok))
    return -1;
  int tokPrec = binopPrecedence[curTok];
  if(tokPrec <= 0) return -1;
  return tokPrec;
}

/// binoprhs
///   ::= ('+' primary)*
static std::unique_ptr<ExprAST> parseBinOpRHS(int exprPrec, std::unique_ptr<ExprAST> lhs) {
  while(true) {
    int tokPrec = getTokPrecedence();

    if(tokPrec < exprPrec)
      return lhs;
    int binOp = curTok;
    getNextToken(); // eat binop

    auto rhs = parsePrimary();
    if(!rhs)
      return nullptr;
    int nextPrec = getTokPrecedence();
    if(tokPrec < nextPrec) {
      rhs = parseBinOpRHS(tokPrec + 1, std::move(rhs));
      if(!rhs)
        return nullptr;
    }

    lhs = llvm::make_unique<BinaryExprAST>(binOp, std::move(lhs), std::move(rhs));
  }
}

/// expression
/// ::= primary binoprhs
static std::unique_ptr<ExprAST> parseExpression() {
  auto lhs = parsePrimary();
  if(!lhs)
    return nullptr;
  return parseBinOpRHS(0, std::move(lhs));
}

/// prototype
/// ::= id '(' id* ')'
static std::unique_ptr<PrototypeAST> parsePrototype() {
  if(curTok != tok_identifier)
    return logErrorP("Expected function name in prototype");

  std::string fnName = identifierStr;
  getNextToken();

  if(curTok != '(')
    return logErrorP("Expected '(' in prototype");
  std::vector<std::string> argNames;
  while(getNextToken() == tok_identifier) {
    argNames.push_back(identifierStr);
  }
  if(curTok != ')') {
    return logErrorP("Expected ')' in prototype");
  }
  getNextToken(); // eat ')'
  return llvm::make_unique<PrototypeAST>(fnName, std::move(argNames));
}

/// definition ::= 'def' prototype expression
static std::unique_ptr<FunctionAST> parseDefinition() {
  getNextToken(); // eat def.
  auto proto = parsePrototype();
  if(!proto)
    return nullptr;

  if(auto e = parseExpression()) {
    return llvm::make_unique<FunctionAST>(std::move(proto), std::move(e));
  }
  return nullptr;
}

/// toplevelexpr ::= expression
static std::unique_ptr<FunctionAST> parseTopLevelExpr() {
  if(auto e = parseExpression()) {
    auto proto = llvm::make_unique<PrototypeAST>("__anon_expr", std::vector<std::string>());
    return llvm::make_unique<FunctionAST>(std::move(proto), std::move(e));
  }
  return nullptr;
}

/// external ::= 'extern' prototype
static std::unique_ptr<PrototypeAST> parseExtern() {
  getNextToken(); // eat extern
  return parsePrototype();
}

//===--------------------------------------------------------------------------------===//
// Code Generation
//===--------------------------------------------------------------------------------===//
static llvm::LLVMContext theContext;
static llvm::IRBuilder<> builder(theContext);
static std::unique_ptr<llvm::Module> theModule;
static std::map<std::string, llvm::Value *> namedValues;


llvm::Value *logErrorV(const char *str) {
  logError(str);
  return nullptr;
}

llvm::Value *NumberExprAST::codegen() {
  return llvm::ConstantFP::get(theContext, llvm::APFloat(val));
}

llvm::Value *VariableExprAST::codegen() {
  llvm::Value *v = namedValues[name];
  if(!v)
    logError("Unknown variable name");
  return v;
}

llvm::Value *BinaryExprAST::codegen() {
  llvm::Value *l = lhs->codegen();
  llvm::Value *r = rhs->codegen();
  if(!l || !r)
    return nullptr;
  switch(op) {
  case '+':
    return builder.CreateFAdd(l, r, "addtmp");
  case '-':
    return builder.CreateFSub(l, r, "subtmp");
  case '*':
    return builder.CreateFMul(l, r, "multmp");
  case '<':
    l = builder.CreateFCmpULT(l, r, "cmptmp");
    return builder.CreateUIToFP(l, llvm::Type::getDoubleTy(theContext), "booltmp");
  default:
    return logErrorV("invalid binary operator");
  }
}

llvm::Value *CallExprAST::codegen() {
  // Look up the name in the global module table.
  llvm::Function *calleeF = theModule->getFunction(callee);
  if(!calleeF)
    return logErrorV("Unknown function referenced");
  if(calleeF->arg_size() != args.size())
    return logErrorV("Incorrect # arguments passed");
  std::vector<llvm::Value *> argsV;
  for(unsigned i = 0, e = args.size(); i != e; ++i) {
    argsV.push_back(args[i]->codegen());
    if(!argsV.back())
      return nullptr;
  }
  return builder.CreateCall(calleeF, argsV, "calltmp");
}

llvm::Function *PrototypeAST::codegen() {
  std::vector<llvm::Type *> doubles(args.size(), llvm::Type::getDoubleTy(theContext));
  llvm::FunctionType *ft = llvm::FunctionType::get(llvm::Type::getDoubleTy(theContext), doubles, false);

  llvm::Function *f = llvm::Function::Create(ft, llvm::Function::ExternalLinkage, name, theModule.get());

  unsigned idx = 0;
  for(auto &arg : f->args()) {
    arg.setName(args[idx++]);
  }
  return f;
}

llvm::Function *FunctionAST::codegen() {
  llvm::Function *theFunction = theModule->getFunction(proto->getName());

  if(!theFunction)
    theFunction = proto->codegen();
  if(!theFunction)
    return nullptr;

  llvm::BasicBlock *bb = llvm::BasicBlock::Create(theContext, "entry", theFunction);
  builder.SetInsertPoint(bb);

  namedValues.clear();
  for(auto &arg : theFunction->args()) {
    namedValues[arg.getName()] = &arg;
  }
  if(llvm::Value *retVal = body->codegen()) {
    builder.CreateRet(retVal);
    llvm::verifyFunction(*theFunction);
    return theFunction;
  }

  theFunction->eraseFromParent();
  return nullptr;
}

//===--------------------------------------------------------------------------------===//
// Top-Level parsing and JIT Driver
//===--------------------------------------------------------------------------------===//

static void handleDefinition() {
  if (auto fnAST = parseDefinition()) {
    if(auto *fnIR = fnAST->codegen()) {
      fprintf(stderr, "Read function definition:");
      fnIR->print(llvm::errs());
      fprintf(stderr, "\n");
    }
  } else {
    getNextToken();
  }
}

static void handleExtern() {
  if(auto protoAST = parseExtern()) {
    if(auto *fnIR = protoAST->codegen()) {
      fprintf(stderr, "Read extern: ");
      fnIR->print(llvm::errs());
      fprintf(stderr, "\n");
    }
  } else {
    getNextToken();
  }
}

static void handleTopLevelExpression() {
  if(auto fnAST = parseTopLevelExpr()) {
    if(auto *fnIR = fnAST->codegen()) {
      fprintf(stderr, "Read top-level expression:");
      fnIR->print(llvm::errs());
      fprintf(stderr, "\n");
    }
  } else {
    getNextToken();
  }
}

static void mainLoop() {
  while(true) {
    fprintf(stderr, "read> ");
    switch(curTok) {
    case tok_eof:
      return;
    case ';':
      getNextToken();
      break;
    case tok_def:
      handleDefinition();
      break;
    case tok_extern:
      handleExtern();
      break;
    default:
      handleTopLevelExpression();
      break;
    }
  }
}

//===--------------------------------------------------------------------------------===//
// Main driver code.
//===--------------------------------------------------------------------------------===//


int main() {
  binopPrecedence['<'] = 10;
  binopPrecedence['+'] = 20;
  binopPrecedence['-'] = 20;
  binopPrecedence['*'] = 40;

  fprintf(stderr, "read> ");
  getNextToken();

  theModule = llvm::make_unique<llvm::Module>("my cool jit", theContext);

  mainLoop();

  theModule->print(llvm::errs(), nullptr);
  return 0;
}
