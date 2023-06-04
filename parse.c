#include "prelude.h"

static struct token current;

void parse_init(char* source) {
  lex_init(source);
  current = lex();
}

void print_expr(struct node* node, int indent) {
  printf("%*s", indent, ""); // print indent many spaces

  switch (node->kind) {
    case ND_NUM: printf("%d\n", node->value); return;
    case ND_ADD: puts("add"); break;
    case ND_SUB: puts("sub"); break;
    case ND_MUL: puts("mul"); break;
    case ND_DIV: puts("div"); break;
    case ND_EQ: puts("eq"); break;
    case ND_LE: puts("le"); break;
    case ND_NEG: puts("neg"); print_expr(node->lhs, indent + 2); return;
    case ND_NOT: puts("not"); print_expr(node->lhs, indent + 2); return;
    case ND_EXPR_STMT: print_expr(node->lhs, indent); return; // XXX
    default: die(0, "impossible");
  }

  print_expr(node->lhs, indent + 2);
  print_expr(node->rhs, indent + 2);
}

static struct node* new_unary(int kind, struct node* lhs) {
  struct node* node = alloc(sizeof (struct node));
  node->kind = kind;
  node->lhs = lhs;
  return node;
}

static struct node* new_binary(int kind, struct node* lhs, struct node* rhs) {
  struct node* node = alloc(sizeof (struct node));
  node->kind = kind;
  node->lhs = lhs;
  node->rhs = rhs;
  return node;
}

static void consume(int kind, char* msg) {
  if (current.kind != kind)
    die(current.line, msg);
  current = lex();
}

static struct node* expr(void);
static struct node* add_expr(void);
static struct node* mul_expr(void);
static struct node* prefix_expr(void);
static struct node* simple_expr(void);

// comparison or equality (non-associative)
static struct node* expr(void) {
  struct node* node = add_expr();

  switch (current.kind) {
    case TK_EQ:
      current = lex();
      return new_binary(ND_EQ, node, mul_expr());
    case TK_NE:
      current = lex();
      return new_unary(ND_NOT, new_binary(ND_EQ, node, mul_expr()));
    case TK_LT:
      current = lex();
      return new_unary(ND_NOT, new_binary(ND_LE, mul_expr(), node));
    case TK_LE:
      current = lex();
      return new_binary(ND_LE, node, mul_expr());
    case TK_GT:
      current = lex();
      return new_unary(ND_NOT, new_binary(ND_LE, node, mul_expr()));
    case TK_GE:
      current = lex();
      return new_binary(ND_LE, mul_expr(), node);
    default: return node;
  }
}

static struct node* add_expr(void) {
  struct node* node = mul_expr();

  for (;;) {
    switch (current.kind) {
      case TK_ADD:
        current = lex();
        node = new_binary(ND_ADD, node, mul_expr());
        break;
      case TK_SUB:
        current = lex();
        node = new_binary(ND_SUB, node, mul_expr());
        break;
      default: return node;
    }
  }
}

static struct node* mul_expr(void) {
  struct node* node = prefix_expr();

  for (;;) {
    switch (current.kind) {
      case TK_MUL:
        current = lex();
        node = new_binary(ND_MUL, node, prefix_expr());
        break;
      case TK_DIV:
        current = lex();
        node = new_binary(ND_DIV, node, prefix_expr());
        break;
      default: return node;
    }
  }
}

static struct node* prefix_expr(void) {
  switch (current.kind) {
    case TK_ADD: current = lex(); return prefix_expr();
    case TK_SUB: current = lex(); return new_unary(ND_NEG, prefix_expr());
    case TK_NOT: current = lex(); return new_unary(ND_NOT, prefix_expr());
    default: return simple_expr();
  }
}

static struct node* simple_expr(void) {
  struct node* node = NULL;

  switch (current.kind) {
    case TK_NUM:
      node = alloc(sizeof (struct node));
      node->kind = ND_NUM;
      node->value = strtoul(current.text, NULL, 10);
      current = lex();
      break;
    case TK_LPAREN:
      current = lex();
      node = expr();
      consume(TK_RPAREN, "expected ')'");
      return node;
    default: die(current.line, "expected a number or '('");
  }

  return node;
}

static struct node* stmt(void) {
  struct node* node = new_unary(ND_EXPR_STMT, expr());
  consume(TK_SEMICOLON, "expected ';'");
  return node;
}

struct node* parse(void) {
  struct node head = {0};
  struct node* node = &head;

  while (current.kind != TK_EOF) {
    node = node->next = stmt();
  }

  return head.next;
}
