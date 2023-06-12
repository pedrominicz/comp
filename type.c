#include "all.h"

struct type* int_ = &(struct type){ TY_INT, {0} };
struct type* bool_ = &(struct type){ TY_BOOL, {0} };

static void eq(struct type* t1, struct type* t2) {
  if (t1->kind != t2->kind) die(-1, "invalid type");

  switch (t1->kind) {
    case TY_INT: case TY_BOOL: return;
    case TY_PTR: eq(t1->ptr, t2->ptr); return;
  }

  impossible();
}

void check(struct expr* e, struct type* t) {
  infer(e);
  eq(e->type, t);
}

static void infer_value(struct value* v) {
  if (v->type) return;

  switch (v->kind) {
    case VAL_INT: v->type = int_; return;
    case VAL_BOOL: v->type = bool_; return;
  }

  impossible();
}

void infer(struct expr* e) {
  if (e->type) return;

  switch (e->kind) {
    case EXPR_VAR:
      if (!e->var->type) {
        e->var->type = int_;
      }
      e->type = e->var->type;
      return;
    case EXPR_VALUE:
      infer_value(e->value);
      e->type = e->value->type;
      return;
    case EXPR_ADD: case EXPR_SUB:
    case EXPR_MUL: case EXPR_DIV:
      check(e->op.l, int_);
      check(e->op.r, int_);
      e->type = int_;
      return;
    case EXPR_EQ: case EXPR_LE:
      infer(e->op.l);
      check(e->op.r, e->op.l->type);
      e->type = bool_;
      return;
    case EXPR_NEG:
      check(e->op.l, int_);
      e->type = int_;
      return;
    case EXPR_NOT:
      check(e->op.l, bool_);
      e->type = bool_;
      return;
    case EXPR_REF:
      if (e->op.l->kind != EXPR_VAR) {
        die(-1, "cannot reference expression");
      }
      infer(e->op.l);
      e->type = alloc(sizeof (struct type));
      e->type->kind = TY_PTR;
      e->type->ptr = e->op.l->type;
      return;
    case EXPR_DEREF:
      infer(e->op.l);
      if (e->op.l->type->kind != TY_PTR) {
        die(-1, "invalid pointer dereference");
      }
      e->type = e->op.l->type->ptr;
      return;
    case EXPR_CALL:
      for (int i = 0; i < MAX_ARGS && e->call.args[i]; ++i) {
        check(e->call.args[i], int_);
      }
      e->type = int_;
      return;
  }

  impossible();
}
