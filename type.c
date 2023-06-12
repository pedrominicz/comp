#include "all.h"

struct type* int_ = &(struct type){ TY_INT, {0} };

static void eq(struct type* t1, struct type* t2) {
  if (t1->kind != t2->kind) die(-1, "invalid type");

  switch (t1->kind) {
    case TY_INT: return;
    case TY_PTR: eq(t1->ptr, t2->ptr); return;
  }

  impossible();
}

void check(struct expr* e, struct type* t) {
  infer(e);
  eq(e->type, t);
}

void infer(struct expr* expr) {
  if (expr->type) return;

  switch (expr->kind) {
    case EXPR_VAR:
      if (!expr->var->type) {
        expr->var->type = int_;
      }
      expr->type = expr->var->type;
      return;
    case EXPR_NUM:
      expr->type = int_;
      return;
    case EXPR_ADD: case EXPR_SUB:
    case EXPR_MUL: case EXPR_DIV:
    case EXPR_EQ: case EXPR_LE:
      check(expr->op.lhs, int_);
      check(expr->op.rhs, int_);
      expr->type = int_;
      return;
    case EXPR_NEG: case EXPR_NOT:
      check(expr->op.lhs, int_);
      expr->type = int_;
      return;
    case EXPR_REF:
      if (expr->op.lhs->kind != EXPR_VAR) {
        die(-1, "cannot reference expression");
      }
      infer(expr->op.lhs);
      expr->type = alloc(sizeof (struct type));
      expr->type->kind = TY_PTR;
      expr->type->ptr = expr->op.lhs->type;
      return;
    case EXPR_DEREF:
      infer(expr->op.lhs);
      if (expr->op.lhs->type->kind != TY_PTR) {
        die(-1, "invalid pointer dereference");
      }
      expr->type = expr->op.lhs->type->ptr;
      return;
    case EXPR_CALL:
      for (int i = 0; i < MAX_ARGS && expr->call.args[i]; ++i) {
        check(expr->call.args[i], int_);
      }
      expr->type = int_;
      return;
  }

  impossible();
}
