#include "all.h"

static struct token current;
static struct token lookahead;
static struct var* locals; // variables created during parsing

void parse_init(char* source) {
  lex_init(source);
  current = lex();
  lookahead = lex();
}

static void next(void) {
  current = lookahead;
  lookahead = lex();
}

void print_var(struct var* var) {
  for (; var; var = var->next) {
    fprintf(stderr, "%s ", var->name);
  }
  fputc('\n', stderr);
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
  if (current.kind != kind) die(current.line, msg);
  struct token previous = current;
  next();
  return previous;
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
    case TK_EQ: next(); return new_binary(ND_EQ, node, mul_expr());
    case TK_NE: next(); return new_unary(ND_NOT, new_binary(ND_EQ, node, mul_expr()));
    case TK_LT: next(); return new_unary(ND_NOT, new_binary(ND_LE, mul_expr(), node));
    case TK_LE: next(); return new_binary(ND_LE, node, mul_expr());
    case TK_GT: next(); return new_unary(ND_NOT, new_binary(ND_LE, node, mul_expr()));
    case TK_GE: next(); return new_binary(ND_LE, mul_expr(), node);
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
      case TK_ADD: next(); node = add(node, mul_expr()); break;
      case TK_SUB: next(); node = sub(node, mul_expr()); break;
      default: return node;
    }
  }
}

static struct node* mul_expr(void) {
  struct node* node = prefix_expr();

  for (;;) {
    switch (current.kind) {
      case TK_MUL: next(); node = new_binary(ND_MUL, node, prefix_expr()); break;
      case TK_DIV: next(); node = new_binary(ND_DIV, node, prefix_expr()); break;
      default: return node;
    }
  }
}

static struct node* prefix_expr(void) {
  switch (current.kind) {
    case TK_ADD: next(); return prefix_expr();
    case TK_SUB: next(); return new_unary(ND_NEG, prefix_expr());
    case TK_NOT: next(); return new_unary(ND_NOT, prefix_expr());
    case TK_REF: next(); return new_unary(ND_REF, prefix_expr());
    case TK_MUL: next(); return new_unary(ND_DEREF, prefix_expr());
    default: return simple_expr();
  }
}

static struct var* find_var(struct token tk) {
  for (struct var* var = locals; var; var = var->next) {
    if (strlen(var->name) == tk.length && !strncmp(var->name, tk.text, tk.length)) {
      return var;
    }
  }

  die(tk.line, "undefined variable '%.*s'", tk.length, tk.text);
  return NULL; // silence warning
}

static struct var* new_var(struct token tk) {
  struct var* var = alloc(sizeof (struct var));
  var->name = strndup(tk.text, tk.length);
  var->type = int_;
  var->next = locals;
  locals = var;
  return var;
}

static struct node* simple_expr(void) {
  if (current.kind == TK_NUM) {
    struct node* node = alloc(sizeof (struct node));
    node->kind = ND_NUM;
    node->value = strtoul(current.text, NULL, 10);
    next();
    return node;
  }

  if (current.kind == TK_IDENT && lookahead.kind == TK_LPAREN) {
    struct node* node = alloc(sizeof (struct node));
    node->kind = ND_CALL;
    node->fn = strndup(current.text, current.length);
    next(); // identifier
    next(); // (

    struct node head = {0};
    struct node* iter = &head;
    while (current.kind != TK_RPAREN) {
      if (iter != &head) consume(TK_COMMA, "expected ')' or ','");
      iter = iter->next = expr();
    }
    next(); // )
    node->args = head.next;

    return node;
  }

  if (current.kind == TK_IDENT) {
    struct node* node = alloc(sizeof (struct node));
    node->kind = ND_VAR;
    node->var = find_var(current);
    next();
    return node;
  }

  if (current.kind == TK_LPAREN) {
    next();
    struct node* node = expr();
    consume(TK_RPAREN, "expected ')'");
    return node;
  }

  die(current.line, "expected an expression");
  return NULL; // silence warning
}

static struct node* stmt(void);
static struct node* let_stmt(void);
static struct node* simple_stmt(void);
static struct node* block_stmt(void);

static struct node* stmt(void) {
  if (current.kind == TK_RETURN) {
    next();

    struct node* node = new_unary(ND_RETURN, expr());
    consume(TK_SEMICOLON, "expected ';'");

    return node;
  }

  if (current.kind == TK_LBRACE) {
    next();
    return block_stmt();
  }

  if (current.kind == TK_IF) {
    next();

    struct node* node = alloc(sizeof (struct node));
    node->kind = ND_IF;

    consume(TK_LPAREN, "expected '('");
    node->cond = expr();
    consume(TK_RPAREN, "expected ')'");

    node->then = stmt();

    if (current.kind == TK_ELSE) {
      next();
      node->else_ = stmt();
    }

    return node;
  }

  if (current.kind == TK_FOR) {
    next();

    struct node* node = alloc(sizeof (struct node));
    node->kind = ND_FOR;

    consume(TK_LPAREN, "expected '('");

    if (current.kind == TK_LET) {
      node->init = let_stmt();
    } else {
      node->init = simple_stmt();
    }

    if (current.kind != TK_SEMICOLON) node->cond = expr();
    consume(TK_SEMICOLON, "expected ';'");

    if (current.kind == TK_IDENT && lookahead.kind == TK_ASSIGN) {
      node->step = alloc(sizeof (struct node));
      node->step->kind = ND_ASSIGN;
      node->step->var = find_var(current);
      next(); // identifier
      next(); // =
      node->step->rhs = expr();
    } else if (current.kind != TK_RPAREN) {
      node->step = new_unary(ND_EXPR_STMT, expr());
    }

    consume(TK_RPAREN, "expected ')'");

    node->body = stmt();

    return node;
  }

  if (current.kind == TK_WHILE) {
    next();

    struct node* node = alloc(sizeof (struct node));
    node->kind = ND_FOR;
    consume(TK_LPAREN, "expected '('");
    node->cond = expr();
    consume(TK_RPAREN, "expected ')'");
    node->body = stmt();

    return node;
  }

  if (current.kind == TK_LET) return let_stmt();

  return simple_stmt();
}

static struct node* let_stmt(void) {
  next(); // TODO create auxiliary match function

  struct node* node = alloc(sizeof (struct node));
  node->kind = ND_ASSIGN;
  // creating a new variable immediately shadows previous definitions too early
  struct token tk = consume(TK_IDENT, "expected an identifier");

  consume(TK_ASSIGN, "expected '='");
  node->rhs = expr(); // may use soon to be shadowed variable
  consume(TK_SEMICOLON, "expected ';'");

  node->var = new_var(tk); // shadow previous definition
  type(node->rhs);
  node->var->type = node->rhs->type;

  return node;
}

static struct node* simple_stmt(void) {
  if (current.kind == TK_SEMICOLON) {
    next();

    struct node* node = alloc(sizeof (struct node));
    node->kind = ND_BLOCK;

    return node;
  }

  if (current.kind == TK_IDENT && lookahead.kind == TK_ASSIGN) {
    struct node* node = alloc(sizeof (struct node));
    node->kind = ND_ASSIGN;
    node->var = find_var(current);
    next(); // identifier
    next(); // =

    node->rhs = expr();
    consume(TK_SEMICOLON, "expected ';'");

    return node;
  }

  struct node* node = new_unary(ND_EXPR_STMT, expr());
  consume(TK_SEMICOLON, "expected ';'");

  return node;
}

static struct node* block_stmt(void) {
  struct node head = {0};
  struct node* iter = &head;
  while (current.kind != TK_RBRACE) {
    iter = iter->next = stmt();
    type(iter);
  }

  next(); // }

  struct node* node = alloc(sizeof (struct node));
  node->kind = ND_BLOCK;
  node->body = head.next;

  return node;
}

struct fn* fn(void) {
  consume(TK_FN, "expected a function");

  struct fn* fn = alloc(sizeof (struct fn));
  fn->name = strndup(current.text, current.length);

  consume(TK_IDENT, "expected an identifier");
  consume(TK_LPAREN, "expected '('");

  locals = NULL;
  if (current.kind != TK_RPAREN) {
    new_var(consume(TK_IDENT, "expected an identifier"));
    while (current.kind != TK_RPAREN) {
      consume(TK_COMMA, "expected ')' or ','");
      new_var(consume(TK_IDENT, "expected an identifier"));
    }
  }
  next(); // )

  // reverse arguments
  fn->args = NULL;
  while (locals) {
    struct var* tmp = locals->next;
    locals->next = fn->args;
    fn->args = locals;
    locals = tmp;
  }
  locals = fn->args;

  consume(TK_LBRACE, "expected '{'");
  fn->body = block_stmt();
  fn->locals = locals;

  return fn;
}

struct fn* parse(void) {
  struct fn head = {0};
  struct fn* iter = &head;
  while (current.kind != TK_EOF) {
    iter = iter->next = fn();
  }

  return head.next;
}
