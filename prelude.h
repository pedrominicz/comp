#include <ctype.h>
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>

void die(int line, char* fmt, ...);
void* alloc(int size);

// lex

enum {
  TK_EOF,
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
  TK_ASSIGN,
  TK_SEMICOLON,
};

struct token {
  int kind;
  char* text;
  int length;
  int line;
};

void print_token(struct token tk);
void lex_init(char* source);
struct token lex(void);

// parse

enum {
  ND_NUM,
  ND_ADD,
  ND_SUB,
  ND_MUL,
  ND_DIV,
  ND_NEG,
  ND_EQ,
  ND_LE,
  ND_NOT,
  ND_EXPR_STMT,
};

struct node {
  int kind;
  struct node* next;
  struct node* lhs;
  struct node* rhs;
  int value;
};

void print_expr(struct node* node, int indent);
void parse_init(char* source);
struct node* parse(void);

// gen

void gen(struct node* node);
