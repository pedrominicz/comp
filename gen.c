#include "all.h"

static struct fn* current_fn;

static int count(void) {
  static int counter = 0;
  return ++counter;
}

static void gen_addr(struct expr* expr);
static void gen_expr(struct expr* expr);

static void gen_addr(struct expr* expr) {
  if (expr->kind == EXPR_VAR) {
    int offset = (expr->var[0] - 'a' + 1) * 8;
    printf("  lea %d(%%rbp), %%rax\n", -offset);  // the stack grows downwards
    return;
  }

  if (expr->kind == EXPR_DEREF) {
    gen_expr(expr->op.lhs);
    return;
  }

  die(-1, "left-hand side of an assignment expression must be a variable");
}

static void gen_expr(struct expr* expr) {
  switch (expr->kind) {
    case EXPR_VAR:
      gen_addr(expr);
      puts("  mov (%rax), %rax");
      return;
    case EXPR_NUM:
      printf("  mov $%d, %%rax\n", expr->value);
      return;
    case EXPR_NEG:
      gen_expr(expr->op.lhs);
      puts("neg %rax");
      return;
    case EXPR_NOT:
      gen_expr(expr->op.lhs);
      puts("  test %rax, %rax");
      puts("  setz %al");
      puts("  movzx %al, %rax");
      return;
    case EXPR_REF:
      gen_addr(expr->op.lhs);
      return;
    case EXPR_DEREF:
      gen_expr(expr->op.lhs);
      puts("  mov (%rax), %rax");
      return;
    case EXPR_CALL:
      die(0, "%s:%d: TODO", __FILE__, __LINE__);
  }

  gen_expr(expr->op.rhs);
  puts("  push %rax");
  gen_expr(expr->op.lhs);
  puts("  pop %rdi");

  switch (expr->kind) {
    case EXPR_ADD:
      puts("  add %rdi, %rax");
      return;
    case EXPR_SUB:
      puts("  sub %rdi, %rax");
      return;
    case EXPR_MUL:
      puts("  imul %rdi, %rax");
      return;
    case EXPR_DIV:
      puts("  cqo");        // rdx:rax := sign-extended rax
      puts("  idiv %rdi");  // rax := rdx:rax / rdi
      return;
    case EXPR_EQ:
      puts("  cmp %rdi, %rax");
      puts("  sete %al");
      puts("  movzx %al, %rax");
      return;
    case EXPR_LE:
      puts("  cmp %rdi, %rax");
      puts("  setle %al");
      puts("  movzx %al, %rax");
      return;
  }

  impossible();
}

static void gen_stmt(struct stmt* stmt) {
  switch (stmt->kind) {
    case STMT_LET:
      die(0, "%s:%d: TODO", __FILE__, __LINE__);
    case STMT_ASSIGN:
      gen_addr(stmt->assign.place);
      puts("  push %rax");
      gen_expr(stmt->assign.value);
      puts("  pop %rdi");
      puts("  mov %rax, (%rdi)");
      return;
    case STMT_EXPR:
      gen_expr(stmt->expr);
      return;
    case STMT_RETURN:
      gen_expr(stmt->return_);
      printf("  jmp .L.return.%s\n", current_fn->name);
      return;
    case STMT_BLOCK:
      for (stmt = stmt->block; stmt; stmt = stmt->next) {
        gen_stmt(stmt);
      }
      return;
    case STMT_IF: {
      int i = count();

      gen_expr(stmt->if_.cond);
      puts("  test %rax, %rax");

      if (stmt->if_.else_) {
        printf("  jz .L.else.%d\n", i);

        gen_stmt(stmt->if_.then);
        printf("  jmp .L.end.%d\n", i);

        printf(".L.else.%d:\n", i);
        gen_stmt(stmt->if_.else_);
      } else {
        printf("  jz .L.end.%d\n", i);

        gen_stmt(stmt->if_.then);
        printf("  jmp .L.end.%d\n", i);
      }

      printf(".L.end.%d:\n", i);
      return;
    }
    case STMT_WHILE: {
      int i = count();

      printf(".L.loop.%d:\n", i);

      if (stmt->while_.cond) {
        gen_expr(stmt->while_.cond);
        puts("  test %rax, %rax");
        printf("  jz .L.end.%d\n", i);
      }

      gen_stmt(stmt->while_.loop);
      printf("  jmp .L.loop.%d\n", i);

      printf(".L.end.%d:\n", i);
      return;
    }
  }

  impossible();
}

void gen(struct fn* prog) {
  puts("  .text");

  for (struct fn* fn = prog; fn; fn = fn->next) {
    printf("  .globl  %s\n", fn->name);
    printf("%s:\n", fn->name);
    puts("  push %rbp");
    puts("  mov %rsp, %rbp");
    puts("  sub $208, %rsp\n");
    puts("  xor %rax, %rax");

    current_fn = fn;
    gen_stmt(fn->body);

    printf(".L.return.%s:\n", fn->name);
    puts("  mov %rbp, %rsp");
    puts("  pop %rbp");
    puts("  ret");
  }
}
