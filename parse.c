#include "prelude.h"

static struct token current;

void print_node(struct node* node, int indent) {
  printf("%*s", indent, ""); // print indent many spaces

  if (node->kind == ND_NUM) {
    printf("%d\n", node->value);
    return;
  }

  if (node->kind == ND_NEG) {
    puts("neg");
    print_node(node->lhs, indent + 2);
    return;
  }

  switch (node->kind) {
    case ND_ADD: puts("add"); break;
    case ND_SUB: puts("sub"); break;
    case ND_MUL: puts("mul"); break;
    case ND_DIV: puts("div"); break;
    default: die(0, "impossible");
  }

  print_node(node->lhs, indent + 2);
  print_node(node->rhs, indent + 2);
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
  current = lex_next_token();
}

static struct node* add_expr(void);
static struct node* mul_expr(void);
static struct node* prefix_expr(void);
static struct node* simple_expr(void);

static struct node* add_expr(void) {
  struct node* node = mul_expr();

  for (;;) {
    switch (current.kind) {
      case TK_ADD:
        current = lex_next_token();
        node = new_binary(ND_ADD, node, mul_expr());
        break;
      case TK_SUB:
        current = lex_next_token();
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
        current = lex_next_token();
        node = new_binary(ND_MUL, node, prefix_expr());
        break;
      case TK_DIV:
        current = lex_next_token();
        node = new_binary(ND_DIV, node, prefix_expr());
        break;
      default: return node;
    }
  }
}

static struct node* prefix_expr(void) {
  if (current.kind == TK_ADD) {
    current = lex_next_token();
    return prefix_expr();
  }

  if (current.kind == TK_SUB) {
    current = lex_next_token();
    struct node* node = alloc(sizeof (struct node));
    node->kind = ND_NEG;
    node->lhs = prefix_expr();
    return node;
  }

  return simple_expr();
}

static struct node* simple_expr(void) {
  struct node* node = NULL;

  switch (current.kind) {
    case TK_NUM:
      node = alloc(sizeof (struct node));
      node->kind = ND_NUM;
      node->value = strtoul(current.text, NULL, 10);
      current = lex_next_token();
      break;
    case TK_LPAREN:
      current = lex_next_token();
      node = add_expr();
      consume(TK_RPAREN, "expected ')'");
      return node;
    default: die(current.line, "expected a number or '('");
  }

  return node;
}

void parse_init(char* source) {
  lex_init(source);
  current = lex_next_token();
}

struct node* parse_expr(void) {
  return add_expr();
}
