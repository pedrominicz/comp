#include "all.h"

void gen(struct fn* prog) {
  puts("  .text");

  for (struct fn* fn = prog; fn; fn = fn->next) {
    printf("  .globl  %s\n", fn->name);
    printf("%s:\n", fn->name);
    puts("  push %rbp");
    puts("  mov %rsp, %rbp");
    puts("  xor %rax, %rax");

    printf(".L.return.%s:\n", fn->name);
    puts("  mov %rbp, %rsp");
    puts("  pop %rbp");
    puts("  ret");
  }
}
