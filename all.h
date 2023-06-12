#include <ctype.h>
#include <stdarg.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define MAX_LOCALS  100
#define MAX_ARGS    6

#define impossible() die(0, "%s:%d: impossible", __FILE__, __LINE__)

struct type;
struct var;
struct expr;
struct stmt;

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

void lex_init(char* source);
struct token lex(void);

struct var {
  char* name;
  struct type* type;
  union {
    int scope;  // used during parsing
    int offset; // used during code generation
  };
};

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
    struct var* var;
    int value; // EXPR_NUM
    struct { // unary and binary expressions
      struct expr* lhs;
      struct expr* rhs;
    } op;
    struct {
      char* fn;
      struct expr* args[MAX_ARGS];
    } call;
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
      struct var* var;
      struct expr* value;
    } let;
    struct {
      struct expr* place;
      struct expr* value;
    } assign;
    struct expr* expr;
    struct expr* return_;
    struct stmt* block;
    struct {
      struct expr* cond;
      struct stmt* then;
      struct stmt* else_;
    } if_;
    struct {
      struct expr* cond;
      struct stmt* loop;
    } while_;
  };
};

struct fn {
  struct fn* next;
  char* name;
  struct var** locals;
  int locals_count;
  int args_count;
  struct stmt* body;
};

// parse

void parse_init(char* source);
struct expr* parse_expr(void);
struct stmt* parse_stmt(void);
struct fn* parse_fn(void);
struct fn* parse(void);

// type

enum {
  TY_INT,
  TY_PTR,
};

struct type {
  int kind;
  union {
    struct type* ptr;
  };
};

void check_type(struct expr* expr, int kind);
void infer_type(struct expr* expr);

// gen

void gen(struct fn* prog);

// print

void print_token(struct token tk);
void print_expr(struct expr* expr);
void print_stmt(struct stmt* stmt);
void print_fn(struct fn* fn);
