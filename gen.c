#include "prelude.h"

static void gen_expr(struct node* node) {
  if (node->kind == ND_NUM) {
    printf("  mov $%d, %%rax\n", node->value);
    return;
  }

  if (node->kind == ND_NEG) {
    gen_expr(node->lhs);
    puts("neg %rax");
    return;
  }

  gen_expr(node->rhs);
  puts("  push %rax");
  gen_expr(node->lhs);
  puts("  pop %rdi");

  switch (node->kind) {
    case ND_ADD:
      puts("  add %rdi, %rax");
      return;
    case ND_SUB:
      puts("  sub %rdi, %rax");
      return;
    case ND_MUL:
      puts("  imul %rdi, %rax");
      return;
    case ND_DIV:
      puts("  cqo");             // rdx:rax := sign-extended rax
      puts("  idiv %rdi, %rax"); // rax := rdx:rax / rdi
      return;
  }

  die(0, "impossible");
}

void gen(struct node* node) {
  puts("  .text");
  puts("  .globl  main");
  puts("main:");
  gen_expr(node);
  puts("  ret");
}
