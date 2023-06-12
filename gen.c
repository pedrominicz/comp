#include "all.h"

static struct fn* current_fn;

// System V Application Binary Interface (section 3.2.3)
static char* regs[] = { "%rdi", "%rsi", "%rdx", "%rcx", "%r8", "%r9" };

static int count(void) {
  static int counter = 0;
  return ++counter;
}

static void gen_addr(struct expr* expr);
static void gen_expr(struct expr* expr);

static void gen_addr(struct expr* expr) {
  if (expr->kind == EXPR_VAR) {
    printf("  lea %d(%%rbp), %%rax\n", expr->var->offset);
    return;
  }

  if (expr->kind == EXPR_DEREF) {
    gen_expr(expr->op.l);
    return;
  }

  impossible();
}

static void gen_value(struct value* v) {
  switch (v->kind) {
    case VAL_INT:
      printf("  mov $%d, %%rax\n", v->int_);
      return;
    case VAL_BOOL:
      if (v->bool_) {
        puts("  mov $1, %al");
        puts("  movzx %al, %eax");
      } else {
        puts("  xor %rax, %rax");
      }
      return;
  }

  impossible();
}

static void gen_expr(struct expr* e) {
  switch (e->kind) {
    case EXPR_VAR:
      gen_addr(e);
      puts("  mov (%rax), %rax");
      return;
    case EXPR_VALUE:
      gen_value(e->value);
      return;
    case EXPR_NEG:
      gen_expr(e->op.l);
      puts("neg %rax");
      return;
    case EXPR_NOT:
      gen_expr(e->op.l);
      puts("  test %rax, %rax");
      puts("  setz %al");
      puts("  movzx %al, %eax");
      return;
    case EXPR_REF:
      gen_addr(e->op.l);
      return;
    case EXPR_DEREF:
      gen_expr(e->op.l);
      puts("  mov (%rax), %rax");
      return;
    case EXPR_CALL: {
      int args = 0;
      while (args < MAX_ARGS && e->call.args[args]) {
        gen_expr(e->call.args[args]);
        puts("  push %rax");
        ++args;
      }

      for (int i = args - 1; i >= 0; --i) {
        printf("  pop %s\n", regs[i]);
      }

      puts("  xor %rax, %rax");
      printf("  call %s\n", e->call.fn);
      return;
    }
  }

  gen_expr(e->op.r);
  puts("  push %rax");
  gen_expr(e->op.l);
  puts("  pop %rdi");

  switch (e->kind) {
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
      puts("  movzx %al, %eax");
      return;
    case EXPR_LE:
      puts("  cmp %rdi, %rax");
      puts("  setle %al");
      puts("  movzx %al, %eax");
      return;
  }

  impossible();
}

static void gen_stmt(struct stmt* stmt) {
  switch (stmt->kind) {
    case STMT_LET: {
      printf("  lea %d(%%rbp), %%rax\n", stmt->let.var->offset);
      puts("  push %rax");
      gen_expr(stmt->let.value);
      puts("  pop %rdi");
      puts("  mov %rax, (%rdi)");
      return;
    }
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

  for (current_fn = prog; current_fn; current_fn = current_fn->next) {
    int stack_size = 0;
    for (int i = 0; i < current_fn->locals_count; ++i) {
      stack_size += 8;
      current_fn->locals[i]->offset = -stack_size; // the stack grows downwards
    }

    printf("  .globl  %s\n", current_fn->name);
    printf("%s:\n", current_fn->name);
    puts("  push %rbp");
    puts("  mov %rsp, %rbp");
    printf("  sub $%d, %%rsp\n", stack_size);
    puts("  xor %rax, %rax");

    for (int i = current_fn->args_count - 1; i >= 0; --i) {
      printf("  mov %s, %d(%%rbp)\n", regs[i], current_fn->locals[i]->offset);
    }

    gen_stmt(current_fn->body);

    printf(".L.return.%s:\n", current_fn->name);
    puts("  mov %rbp, %rsp");
    puts("  pop %rbp");
    puts("  ret");
  }
}
