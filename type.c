#include "all.h"

static struct type* int_ = &(struct type){ TY_NUM };

void infer_expr(struct expr* expr) {
  expr->type = int_;
}

