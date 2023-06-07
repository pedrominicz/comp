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
    case ND_ADD: case ND_SUB:
    case ND_MUL: case ND_DIV:
    case ND_NEG: case ND_ASSIGN:
      node->type = node->lhs->type;
      break;
    case ND_VAR: case ND_NUM:
    case ND_EQ: case ND_LE:
    case ND_NOT:
      node->type = int_;
      break;
    case ND_REF:
      node->type = alloc(sizeof (struct type));
      node->type->kind = TY_REF;
      node->type->base = node->lhs->type;
      break;
    case ND_DEREF:
      if (node->lhs->type->kind == TY_REF) {
        node->type = node->lhs->type->base;
      } else {
        node->type = int_;
      }
      break;
  }

  for (node = node->next; node; node = node->next) {
    type(node);
  }
}
