//
// Created by 吴凡 on 2017/5/10.
//

#include "llvm/ADT/STLExtras.h"

#include <iostream>
#include <memory>
#include <vector>
#include <string>
#include <map>


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


//===--------------------------------------------------------------------------------===//
// Lexer
//===--------------------------------------------------------------------------------===//

enum Token {
  tok_eof = -1,

  // commands

  tok_def = -2,
  tok_extern = -3,

  // primary

  tok_identifier = -4,
  tok_number = -5,
};

static std::string identifierStr;
static double numVal;

static int gettok() {
  static int lastChar = ' ';

  while (isspace(lastChar)) {
    lastChar = getchar();
  }

  if (isalpha(lastChar)) { // identifier: [a-zA-Z][a-zA-Z0-9]*
    identifierStr = lastChar;
    while (isalnum(lastChar = getchar())) {
      identifierStr += lastChar;
    }

    if (identifierStr == "def") {
      return tok_def;
    }
    if (identifierStr == "extern") {
      return tok_extern;
    }
    return tok_identifier;
  }

  if (isdigit(lastChar) || lastChar == '.') { // number: [0-9.]+
    std::string numStr;
    do {
      numStr += lastChar;
      lastChar = getchar();
    } while (isdigit(lastChar) || lastChar == '.');

    numVal = strtod(numStr.c_str(), 0);
    return tok_number;
  }

  if (lastChar == '#') { // comment until end of line.
    do
      lastChar = getchar();
    while (lastChar != EOF && lastChar != '\n' && lastChar != '\r');
    if (lastChar != EOF) {
      return gettok();
    }
  }

  if (lastChar == EOF)
    return tok_eof;

  int thisChar = lastChar;
  lastChar = getchar();
  return thisChar;
}

//===--------------------------------------------------------------------------------===//
// Abstract Syntax Tree (aka Parse Tree)
//===--------------------------------------------------------------------------------===//

namespace {

/// ExprAST - Base class for all expression nodes.
class ExprAST {
public:
  virtual ~ExprAST() = default;
};

/// NumberExprAST - Expression class for numeric literals like "1.0".
class NumberExprAST : public ExprAST {
  double val;
public:
  NumberExprAST(double val) : val(val) {}
};

/// VariableExprAST - Expression class for referencing a variable, like "a".
class VariableExprAST : public ExprAST {
  std::string name;
public:
  VariableExprAST(const std::string &name) : name(name) {}
};

/// BinaryExprAST - Expression class for a binary operator
class BinaryExprAST : public ExprAST {
  char op;
  std::unique_ptr<ExprAST> lhs, rhs;
public:
  BinaryExprAST(char op, std::unique_ptr<ExprAST> lhs, std::unique_ptr<ExprAST> rhs) :
      op(op), lhs(std::move(lhs)), rhs(std::move(rhs)) {}
};

/// CallExprAST - Expression class for function calls
class CallExprAST : public ExprAST {
  std::string callee;
  std::vector<std::unique_ptr<ExprAST>> args;
public:
  CallExprAST(const std::string &callee, std::vector<std::unique_ptr<ExprAST>> args) :
      callee(callee), args(std::move(args)) {}
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
};

/// FunctionAST - This class represents a function definition itself.
class FunctionAST {
  std::unique_ptr<PrototypeAST> proto;
  std::unique_ptr<ExprAST> body;

public:
  FunctionAST(std::unique_ptr<PrototypeAST> proto, std::unique_ptr<ExprAST> body) :
      proto(std::move(proto)), body(std::move(body)) {}
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
// Top-Level parsing
//===--------------------------------------------------------------------------------===//

static void handleDefinition() {
  if(parseDefinition()) {
    fprintf(stderr, "Parsed a function definition.\n");
  } else {
    getNextToken();
  }
}

static void handleExtern() {
  if(parseExtern()) {
    fprintf(stderr, "Parsed an extern.\n");
  } else {
    getNextToken();
  }
}

static void handleTopLevelExpression() {
  if(parseTopLevelExpr()) {
    fprintf(stderr, "Parsed a top-level expr\n");
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

  mainLoop();

  return 0;
}

