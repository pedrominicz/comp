#include "all.h"

static void gen_addr(struct node* node) {
  if (node->kind == ND_VAR) {
    printf("  lea %d(%%rbp), %%rax\n", node->var->offset);
    return;
  }

  die(0, "left-hand side of an assignment expression must be a variable");
}

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
    case ND_VAR:
      gen_addr(node);
      puts("  mov (%rax), %rax");
      return;
    case ND_ASSIGN:
      gen_addr(node->lhs);
      puts("  push %rax");
      gen_expr(node->rhs);
      puts("  pop %rdi");
      puts("  mov %rax, (%rdi)");
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
      puts("  cqo");        // rdx:rax := sign-extended rax
      puts("  idiv %rdi");  // rax := rdx:rax / rdi
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
  switch (node->kind) {
    case ND_EXPR_STMT:
      gen_expr(node->lhs);
      return;
    case ND_RETURN:
      gen_expr(node->lhs);
      puts("  jmp .L.return");
      return;
    case ND_BLOCK:
      for (node = node->body; node; node = node->next) {
        gen_stmt(node);
      }
      return;
  }

  die(0, "impossible");
}

void gen(struct fn* fn) {
  int offset = 0;
  for (struct var* var = fn->locals; var; var = var->next) {
    offset += 8;
    var->offset = -offset; // the stack grows downwards
  }
  fn->stack_size = offset;

  puts("  .text");
  puts("  .globl  main");
  puts("main:");
  puts("  push %rbp");
  puts("  mov %rsp, %rbp");
  printf("  sub $%d, %%rsp\n", fn->stack_size);
  puts("  xor %rax, %rax");

  gen_stmt(fn->body);

  puts(".L.return:");
  puts("  mov %rbp, %rsp");
  puts("  pop %rbp");
  puts("  ret");
}
