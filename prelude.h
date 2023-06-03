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
};

struct token {
  int kind;
  char* text;
  int length;
  int line;
};

void lex_init(char* source);
struct token lex_next_token(void);

// parse

enum {
  ND_NUM,
  ND_ADD,
  ND_SUB,
  ND_MUL,
  ND_DIV,
  ND_NEG,
};

struct node {
  int kind;
  struct node* lhs;
  struct node* rhs;
  int value;
};

void print_node(struct node* node, int indent);
void parse_init(char* source);
struct node* parse_expr(void);

// gen

void gen(struct node* node);
