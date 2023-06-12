#include "all.h"

static struct type* int_ = &(struct type){ TY_INT, {0} };

void check_type(struct expr* expr, int kind) {
  infer_type(expr);

  if (expr->type->kind != kind) {
    die(-1, "invalid type");
  }
}

void infer_type(struct expr* expr) {
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
      check_type(expr->op.lhs, TY_INT);
      check_type(expr->op.rhs, TY_INT);
      expr->type = int_;
      return;
    case EXPR_NEG: case EXPR_NOT:
      check_type(expr->op.lhs, TY_INT);
      expr->type = int_;
      return;
    case EXPR_REF:
      infer_type(expr->op.lhs);
      expr->type = alloc(sizeof (struct type));
      expr->type->kind = TY_PTR;
      expr->type->ptr = expr->op.lhs->type;
      return;
    case EXPR_DEREF:
      check_type(expr->op.lhs, TY_PTR);
      expr->type = expr->op.lhs->type->ptr;
      return;
    case EXPR_CALL:
      for (int i = 0; i < MAX_ARGS && expr->call.args[i]; ++i) {
        check_type(expr->call.args[i], TY_INT);
      }
      expr->type = int_;
      return;
  }

  impossible();
}

