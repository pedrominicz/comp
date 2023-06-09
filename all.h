#include <ctype.h>
#include <stdarg.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

void die(int line, char* fmt, ...);
void* alloc(int size);

// lex

enum {
  TK_EOF,
  TK_IDENT,
  TK_NUM,
  TK_LPAREN,
  TK_RPAREN,
  TK_ADD,
  TK_SUB,
  TK_MUL,
  TK_DIV,
  TK_EQ,
  TK_NE,
  TK_LT,
  TK_LE,
  TK_GT,
  TK_GE,
  TK_NOT,
  TK_REF, // &
  TK_ASSIGN,
  TK_SEMICOLON,
  TK_COMMA,
  TK_LBRACKET,
  TK_RBRACKET,
  TK_RETURN,
  TK_LBRACE,
  TK_RBRACE,
  TK_IF,
  TK_ELSE,
  TK_FOR,
  TK_WHILE,
  TK_LET,
  TK_FN,
};

struct token {
  int kind;
  int line;
  char* str;
};

void print_token(struct token tk);
void lex_init(char* source);
struct token lex(void);

enum {
  EXPR_VAR,
  EXPR_NUM,
  // binary expressions
  EXPR_ADD, EXPR_SUB, EXPR_MUL, EXPR_DIV, EXPR_EQ, EXPR_LE,
  // unary expressions
  EXPR_NEG, EXPR_NOT, EXPR_REF, EXPR_DEREF,
  EXPR_CALL,
};

struct expr {
  int kind;
  struct type* type;
  union {
    char* var; // EXPR_VAR
    int value; // EXPR_NUM
    struct {
      struct expr* lhs;
      struct expr* rhs;
    } binary_expr;
    struct {
      struct expr* lhs; // isn't it weird that the operand is called lhs?
    } unary_expr;
    struct {
      char* fn;
      struct expr* args[6];
    } call_expr;
  };
};

enum {
  STMT_LET,
  STMT_ASSIGN,
  STMT_EXPR,
  STMT_RETURN,
  STMT_BLOCK,
  STMT_IF,
  STMT_WHILE,
};

struct stmt {
  int kind;
  struct stmt* next;
  union {
    struct {
      char* var;
      struct expr* value;
    } let_stmt, assign_stmt;
    struct expr* expr; // expression statement
    struct {
      struct expr* value;
    } return_stmt;
    struct {
      struct stmt* body;
    } block_stmt;
    struct {
      struct expr* cond;
      struct stmt* then;
      struct stmt* else_;
    } if_stmt;
    struct {
      struct expr* cond;
      struct stmt* loop;
    } while_stmt;
  };
};

struct fn {
  struct fn* next;
  char* name;
  char* args[6];
  struct stmt* body;
};

// print

void print_expr(struct expr* expr);
void print_stmt(struct stmt* stmt);
void print_fn(struct fn* fn);

// parse

void parse_init(char* source);
struct expr* parse_expr(void);
struct stmt* parse_stmt(void);
struct fn* parse_fn(void);
struct fn* parse(void);

// gen

void gen(struct fn* fn);
