#include "all.h"

struct type* int_ = &(struct type){ TY_INT, NULL };

void type(struct node* node) {
  if (!node || node->type) return;

  type(node->lhs);
  type(node->rhs);
  type(node->body);
  type(node->cond);
  type(node->then);
  type(node->else_);
  type(node->init);
  type(node->step);

  switch (node->kind) {
    case ND_VAR:
      node->type = node->var->type;
      break;
    case ND_ADD: case ND_SUB:
    case ND_MUL: case ND_DIV:
    case ND_NEG:
      node->type = node->lhs->type;
      break;
    case ND_NUM: case ND_NOT:
    case ND_EQ: case ND_LE:
    case ND_CALL:
      node->type = int_;
      break;
    case ND_REF:
      node->type = alloc(sizeof (struct type));
      node->type->kind = TY_REF;
      node->type->base = node->lhs->type;
      break;
    case ND_DEREF:
      if (node->lhs->type->kind != TY_REF) {
        die(0, "invalid dereference"); // TODO line number
      }
      node->type = node->lhs->type->base;
      break;
  }

  for (node = node->next; node; node = node->next) {
    type(node);
  }
}
