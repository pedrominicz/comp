#include "all.h"

static struct token current;
static struct var* locals; // variables created during parsing

void parse_init(char* source) {
  lex_init(source);
  current = lex();
}

static void print_indented_expr(struct node* node, int indent) {
  if (!node) return;

  fprintf(stderr, "%*s", indent, ""); // print indent many spaces

  switch (node->kind) {
    case ND_VAR: fprintf(stderr, "var (%s)", node->var->name); break;
    case ND_NUM: fprintf(stderr, "num (%d)", node->value); break;
    case ND_ADD: fputs("add", stderr); break;
    case ND_SUB: fputs("sub", stderr); break;
    case ND_MUL: fputs("mul", stderr); break;
    case ND_DIV: fputs("div", stderr); break;
    case ND_EQ: fputs("eq", stderr); break;
    case ND_LE: fputs("le", stderr); break;
    case ND_NEG: fputs("neg", stderr); break;
    case ND_NOT: fputs("not", stderr); break;
    case ND_REF: fputs("ref", stderr); break;
    case ND_DEREF: fputs("deref", stderr); break;
    case ND_ASSIGN: fputs("assign", stderr); break;
    default: die(0, "%s:%d: impossible", __FILE__, __LINE__);
  }

  fputc('\n', stderr);

  print_indented_expr(node->lhs, indent + 2);
  print_indented_expr(node->rhs, indent + 2);
}

void print_expr(struct node* node) {
  print_indented_expr(node, 0);
}

static struct node* new_unary(int kind, struct node* lhs) {
  struct node* node = alloc(sizeof (struct node));
  node->kind = kind;
  node->lhs = lhs; // isn't it weird that the operand goes into lhs?
  return node;
}

static struct node* new_binary(int kind, struct node* lhs, struct node* rhs) {
  struct node* node = alloc(sizeof (struct node));
  node->kind = kind;
  node->lhs = lhs;
  node->rhs = rhs;
  return node;
}

static struct token consume(int kind, char* msg) {
  if (current.kind != kind)
    die(current.line, msg);
  struct token previous = current;
  current = lex();
  return previous;
}

static struct node* expr(void);
static struct node* eq_expr(void);
static struct node* add_expr(void);
static struct node* mul_expr(void);
static struct node* prefix_expr(void);
static struct node* simple_expr(void);

// assignment (non-associative)
static struct node* expr(void) {
  struct node* node = eq_expr();

  if (current.kind == TK_ASSIGN) {
    current = lex();
    return new_binary(ND_ASSIGN, node, eq_expr());
  }

  return node;
}

// comparison or equality (non-associative)
static struct node* eq_expr(void) {
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

static struct node* add(struct node* lhs, struct node* rhs) {
  type(lhs);
  type(rhs);

  if (lhs->type->kind == TY_INT && rhs->type->kind == TY_INT) {
    return new_binary(ND_ADD, lhs, rhs);
  }

  if (lhs->type->kind == TY_REF && rhs->type->kind == TY_REF) {
    die(0, "invalid operands for addition"); // TODO line number
  }

  // canonicalize `num + ref` to `ref + num`
  if (lhs->type->kind == TY_INT && rhs->type->kind == TY_REF) {
    struct node* tmp = lhs;
    lhs = rhs;
    rhs = tmp;
  }

  struct node* sizeof_int = alloc(sizeof (struct node));
  sizeof_int->kind = ND_NUM;
  sizeof_int->value = 8;

  rhs = new_binary(ND_MUL, rhs, sizeof_int);
  return new_binary(ND_ADD, lhs, rhs);
}

static struct node* sub(struct node* lhs, struct node* rhs) {
  type(lhs);
  type(rhs);

  if (lhs->type->kind == TY_INT && rhs->type->kind == TY_INT) {
    return new_binary(ND_SUB, lhs, rhs);
  }

  if (lhs->type->kind == TY_INT && rhs->type->kind == TY_REF) {
    die(0, "invalid operands for subtraction"); // TODO line number
  }

  struct node* sizeof_int = alloc(sizeof (struct node));
  sizeof_int->kind = ND_NUM;
  sizeof_int->value = 8;

  if (rhs->type->kind == TY_INT) {
    // ref - int
    rhs = new_binary(ND_MUL, rhs, sizeof_int);
    return new_binary(ND_SUB, lhs, rhs);
  } else {
    // ref - ref
    struct node* node = new_binary(ND_SUB, lhs, rhs);
    node->type = int_; // if not set, `type` will think this expression results
                       // in a reference
    return new_binary(ND_DIV, node, sizeof_int);
  }
}

static struct node* add_expr(void) {
  struct node* node = mul_expr();

  for (;;) {
    switch (current.kind) {
      case TK_ADD: current = lex(); node = add(node, mul_expr()); break;
      case TK_SUB: current = lex(); node = sub(node, mul_expr()); break;
      default: return node;
    }
  }
}

static struct node* mul_expr(void) {
  struct node* node = prefix_expr();

  for (;;) {
    int kind;

    switch (current.kind) {
      case TK_MUL: kind = ND_MUL; break;
      case TK_DIV: kind = ND_DIV; break;
      default: return node;
    }

    current = lex();
    node = new_binary(kind, node, prefix_expr());
  }
}

static struct node* prefix_expr(void) {
  switch (current.kind) {
    case TK_ADD: current = lex(); return prefix_expr();
    case TK_SUB: current = lex(); return new_unary(ND_NEG, prefix_expr());
    case TK_NOT: current = lex(); return new_unary(ND_NOT, prefix_expr());
    case TK_REF: current = lex(); return new_unary(ND_REF, prefix_expr());
    case TK_MUL: current = lex(); return new_unary(ND_DEREF, prefix_expr());
    default: return simple_expr();
  }
}

static struct var* find_var(struct token tk) {
  for (struct var* var = locals; var; var = var->next) {
    if (strlen(var->name) == tk.length && !strncmp(var->name, tk.text, tk.length)) {
      return var;
    }
  }

  return NULL;
}

static struct var* new_var(struct token tk) {
  struct var* var = find_var(tk);
  if (var) return var;

  var = alloc(sizeof (struct var));
  var->name = strndup(tk.text, tk.length);
  var->next = locals;
  locals = var;

  return var;
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
    case TK_IDENT:
      node = alloc(sizeof (struct node));
      node->kind = ND_VAR;
      node->var = find_var(current);
      if (!node->var) die(current.line, "undefined variable '%.*s'", current.length, current.text);
      current = lex();
      break;
    case TK_LPAREN:
      current = lex();
      node = expr();
      consume(TK_RPAREN, "expected ')'");
      break;
    default: die(current.line, "expected an expression");
  }

  return node;
}

static struct node* stmt(void);
static struct node* let_stmt(void);
static struct node* expr_stmt(void);
static struct node* block_stmt(void);

static struct node* stmt(void) {
  if (current.kind == TK_RETURN) {
    current = lex();

    struct node* node = new_unary(ND_RETURN, expr());
    consume(TK_SEMICOLON, "expected ';'");

    return node;
  }

  if (current.kind == TK_LBRACE) {
    current = lex();
    return block_stmt();
  }

  if (current.kind == TK_IF) {
    current = lex();

    struct node* node = alloc(sizeof (struct node));
    node->kind = ND_IF;

    consume(TK_LPAREN, "expected '('");
    node->cond = expr();
    consume(TK_RPAREN, "expected ')'");

    node->then = stmt();

    if (current.kind == TK_ELSE) {
      current = lex();
      node->else_ = stmt();
    }

    return node;
  }

  if (current.kind == TK_FOR) {
    current = lex();

    struct node* node = alloc(sizeof (struct node));
    node->kind = ND_FOR;

    consume(TK_LPAREN, "expected '('");

    if (current.kind == TK_LET) {
      node->init = let_stmt();
    } else {
      node->init = expr_stmt();
    }

    if (current.kind != TK_SEMICOLON) node->cond = expr();
    consume(TK_SEMICOLON, "expected ';'");
    if (current.kind != TK_RPAREN) node->step = expr();
    consume(TK_RPAREN, "expected ')'");

    node->body = stmt();

    return node;
  }

  if (current.kind == TK_WHILE) {
    current = lex();

    struct node* node = alloc(sizeof (struct node));
    node->kind = ND_FOR;
    consume(TK_LPAREN, "expected '('");
    node->cond = expr();
    consume(TK_RPAREN, "expected ')'");
    node->body = stmt();

    return node;
  }

  if (current.kind == TK_LET) return let_stmt();

  return expr_stmt();
}

static struct node* let_stmt(void) {
  current = lex();

  struct node* lhs = alloc(sizeof (struct node));
  lhs->kind = ND_VAR;
  lhs->var = new_var(consume(TK_IDENT, "expected an identifier"));

  consume(TK_ASSIGN, "expected '='");
  struct node* rhs = expr();
  consume(TK_SEMICOLON, "expected ';'");

  type(rhs);
  lhs->var->type = rhs->type;

  return new_unary(ND_EXPR_STMT, new_binary(ND_ASSIGN, lhs, rhs));
}

static struct node* expr_stmt(void) {
  if (current.kind == TK_SEMICOLON) {
    current = lex();

    struct node* node = alloc(sizeof (struct node));
    node->kind = ND_BLOCK;

    return node;
  }

  struct node* node = new_unary(ND_EXPR_STMT, expr());
  consume(TK_SEMICOLON, "expected ';'");

  return node;
}

static struct node* block_stmt(void) {
  struct node head = {0};
  struct node* node = &head;

  while (current.kind != TK_RBRACE) {
    node = node->next = stmt();
    type(node);
  }

  current = lex();

  node = alloc(sizeof (struct node));
  node->kind = ND_BLOCK;
  node->body = head.next;

  return node;
}

struct fn* parse(void) {
  consume(TK_LBRACE, "expected '{'");

  struct fn* fn = alloc(sizeof (struct fn));
  fn->body = block_stmt();
  fn->locals = locals;

  return fn;
}
