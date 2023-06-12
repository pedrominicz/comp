#include "all.h"

static struct token current;
static struct token lookahead;

static struct fn* current_fn;

static struct {
  struct var* locals[MAX_LOCALS];
  int count;
  int depth;
} scope;

void parse_init(char* source) {
  lex_init(source);
  current = lex();
  lookahead = lex();
}

static void next(void) {
  current = lookahead;
  lookahead = lex();
}

static bool match(int kind) {
  if (current.kind == kind) {
    next();
    return true;
  }
  return false;
}

static struct token consume(int kind, char* msg) {
  if (current.kind != kind) die(current.line, msg);
  struct token previous = current;
  next();
  return previous;
}

static struct var* find_var() {
  for (int i = scope.count - 1; i >= 0; --i) {
    if (strcmp(scope.locals[i]->name, current.str) == 0) {
      return scope.locals[i];
    }
  }

  die(current.line, "undefined variable '%s'", current.str);
  return NULL; // silence warning
}

static struct var* new_var(struct token tk) {
  if (current_fn->locals_count >= MAX_LOCALS) {
    die(tk.line, "too many local variables (max %d)", MAX_LOCALS);
  }

  struct var* var = alloc(sizeof (struct var));
  var->name = tk.str;
  var->scope = scope.depth;

  current_fn->locals[current_fn->locals_count++] = var;
  scope.locals[scope.count++] = var;
  return var;
}

static struct expr* new_binary(int kind, struct expr* lhs, struct expr* rhs) {
  struct expr* expr = alloc(sizeof (struct expr));
  expr->kind = kind;
  expr->op.lhs = lhs;
  expr->op.rhs = rhs;
  return expr;
}


static struct expr* new_unary(int kind, struct expr* lhs) {
  return new_binary(kind, lhs, NULL);
}

static struct expr* simple_expr(void) {
  if (current.kind == TK_NUM) {
    struct expr* expr = alloc(sizeof (struct expr));
    expr->kind = EXPR_NUM;
    expr->value = strtoul(current.str, NULL, 10);
    next();
    return expr;
  }

  if (current.kind == TK_IDENT && lookahead.kind == TK_LPAREN) {
    struct expr* expr = alloc(sizeof (struct expr));
    expr->kind = EXPR_CALL;
    expr->call.fn = current.str;
    next(); // identifier
    next(); // (

    if (current.kind != TK_RPAREN) {
      expr->call.args[0] = parse_expr();
      for (int i = 1; i < MAX_ARGS && current.kind != TK_RPAREN; ++i) {
        consume(TK_COMMA, "expected ')' or ','");
        expr->call.args[i] = parse_expr();
      }
    }
    consume(TK_RPAREN, "expected ')'");

    return expr;
  }

  if (current.kind == TK_IDENT) {
    struct expr* expr = alloc(sizeof (struct expr));
    expr->kind = EXPR_VAR;
    expr->var = find_var();
    next();
    return expr;
  }

  if (current.kind == TK_LPAREN) {
    next();
    struct expr* expr = parse_expr();
    consume(TK_RPAREN, "expected ')'");
    return expr;
  }

  die(current.line, "expected an expression");
  return NULL; // silence warning
}

static struct expr* prefix_expr(void) {
  switch (current.kind) {
    // XXX unary + not supported
    case TK_SUB: next(); return new_unary(EXPR_NEG, prefix_expr());
    case TK_NOT: next(); return new_unary(EXPR_NOT, prefix_expr());
    case TK_REF: next(); return new_unary(EXPR_REF, prefix_expr());
    case TK_MUL: next(); return new_unary(EXPR_DEREF, prefix_expr());
    default: return simple_expr();
  }
}

static struct expr* mul_expr(void) {
  struct expr* expr = prefix_expr();
  for (;;) {
    switch (current.kind) {
      case TK_MUL: next(); expr = new_binary(EXPR_MUL, expr, prefix_expr()); break;
      case TK_DIV: next(); expr = new_binary(EXPR_DIV, expr, prefix_expr()); break;
      default: return expr;
    }
  }
}

static struct expr* add_expr(void) {
  struct expr* expr = mul_expr();
  for (;;) {
    switch (current.kind) {
      case TK_ADD: next(); expr = new_binary(EXPR_ADD, expr, mul_expr()); break;
      case TK_SUB: next(); expr = new_binary(EXPR_SUB, expr, mul_expr()); break;
      default: return expr;
    }
  }
}

// comparison or equality (non-associative)
struct expr* parse_expr(void) {
  struct expr* expr = add_expr();
  switch (current.kind) {
    case TK_EQ: next(); return new_binary(EXPR_EQ, expr, add_expr());
    case TK_NE: next(); return new_unary(EXPR_NOT, new_binary(EXPR_EQ, expr, add_expr()));
    case TK_LT: next(); return new_unary(EXPR_NOT, new_binary(EXPR_LE, add_expr(), expr));
    case TK_LE: next(); return new_binary(EXPR_LE, expr, add_expr());
    case TK_GT: next(); return new_unary(EXPR_NOT, new_binary(EXPR_LE, expr, add_expr()));
    case TK_GE: next(); return new_binary(EXPR_LE, add_expr(), expr);
    default: return expr;
  }
}

static struct stmt* block_stmt(void) {
  ++scope.depth;

  struct stmt head = {0};
  struct stmt* iter = &head;
  while (current.kind != TK_RBRACE) {
    iter = iter->next = parse_stmt();
  }
  next(); // }

  struct stmt* stmt = alloc(sizeof (struct stmt));
  stmt->kind = STMT_BLOCK;
  stmt->block = head.next;

  int i = scope.count - 1;
  for (; i > 0; --i) {
    if (scope.locals[i]->scope < scope.depth) break;
  }
  scope.count = i + 1;
  --scope.depth;

  return stmt;
}

struct stmt* parse_stmt(void) {
  if (match(TK_LET)) {
    struct stmt* stmt = alloc(sizeof (struct stmt));
    stmt->kind = STMT_LET;

    struct token name = consume(TK_IDENT, "expected an identifier");
    consume(TK_ASSIGN, "expected '='");

    stmt->let.value = parse_expr(); // can use a previous definition of `name`
    infer_type(stmt->let.value);
    consume(TK_SEMICOLON, "expected ';'");
    stmt->let.var = new_var(name);  // shadow previous definition
    stmt->let.var->type = stmt->let.value->type;

    return stmt;
  }

  if (match(TK_RETURN)) {
    struct stmt* stmt = alloc(sizeof (struct stmt));
    stmt->kind = STMT_RETURN;

    stmt->return_ = parse_expr();
    check_type(stmt->return_, TY_INT);
    consume(TK_SEMICOLON, "expected ';'");

    return stmt;
  }

  if (match(TK_LBRACE)) return block_stmt();

  if (match(TK_IF)) {
    struct stmt* stmt = alloc(sizeof (struct stmt));
    stmt->kind = STMT_IF;

    consume(TK_LPAREN, "expected '('");
    stmt->if_.cond = parse_expr();
    check_type(stmt->if_.cond, TY_INT);
    consume(TK_RPAREN, "expected ')'");

    stmt->if_.then = parse_stmt();
    if (match(TK_ELSE)) stmt->if_.else_ = parse_stmt();

    return stmt;
  }

  if (match(TK_WHILE)) {
    struct stmt* stmt = alloc(sizeof (struct stmt));
    stmt->kind = STMT_WHILE;

    consume(TK_LPAREN, "expected '('");
    stmt->while_.cond = parse_expr();
    check_type(stmt->while_.cond, TY_INT);
    consume(TK_RPAREN, "expected ')'");

    stmt->while_.loop = parse_stmt();

    return stmt;
  }

  struct expr* expr = parse_expr();
  infer_type(expr);

  if (match(TK_ASSIGN)) {
    struct stmt* stmt = alloc(sizeof (struct stmt));
    stmt->kind = STMT_ASSIGN;

    stmt->assign.place = expr;

    stmt->assign.value = parse_expr();
    infer_type(stmt->assign.value);
    consume(TK_SEMICOLON, "expected ';'");

    return stmt;
  }

  struct stmt* stmt = alloc(sizeof (struct stmt));
  stmt->kind = STMT_EXPR;

  stmt->expr = expr;
  consume(TK_SEMICOLON, "expected ';'");

  return stmt;
}

struct fn* parse_fn(void) {
  consume(TK_FN, "expected a function");

  current_fn = alloc(sizeof (struct fn));
  current_fn->name = current.str;

  current_fn->locals = alloc(sizeof (struct var) * MAX_LOCALS);
  scope.count = 0;
  scope.depth = 0;

  consume(TK_IDENT, "expected an identifier");
  consume(TK_LPAREN, "expected '('");
  if (current.kind != TK_RPAREN) {
    new_var(consume(TK_IDENT, "expected an identifier"));
    current_fn->args_count = 1;
    while (current_fn->args_count < MAX_ARGS && current.kind != TK_RPAREN) {
      consume(TK_COMMA, "expected ')' or ','");
      new_var(consume(TK_IDENT, "expected an identifier"));
      ++current_fn->args_count;
    }
  }
  consume(TK_RPAREN, "expected ')'");

  consume(TK_LBRACE, "expected '{'");
  current_fn->body = block_stmt();

  return current_fn;
}

struct fn* parse(void) {
  struct fn head = {0};
  struct fn* iter = &head;
  while (current.kind != TK_EOF) {
    iter = iter->next = parse_fn();
  }

  return head.next;
}
