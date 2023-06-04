#include "prelude.h"

static void gen_expr(struct node* node) {
  switch (node->kind) {
    case ND_NUM:
      printf("  mov $%d, %%rax\n", node->value);
      return;
    case ND_NEG:
      gen_expr(node->lhs);
      puts("neg %rax");
      return;
    case ND_NOT:
      gen_expr(node->lhs);
      puts("  test %rax, %rax");
      puts("  setz %al");
      puts("  movzx %al, %rax");
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
    case ND_EQ:
      puts("  cmp %rdi, %rax");
      puts("  sete %al");
      puts("  movzx %al, %rax");
      return;
    case ND_LE:
      puts("  cmp %rdi, %rax");
      puts("  setle %al");
      puts("  movzx %al, %rax");
      return;
  }

  die(0, "impossible");
}

static void gen_stmt(struct node* node) {
  if (node->kind == ND_EXPR_STMT) {
    gen_expr(node->lhs);
    return;
  }

  die(0, "impossible");
}

void gen(struct node* node) {
  puts("  .text");
  puts("  .globl  main");
  puts("main:");

  for (; node; node = node->next) {
    gen_stmt(node);
  }

  puts("  ret");
}
