#include <stdio.h>

#define int long long

// Remember that the stack grows downwards. That is `push %rax` does the
// following:
//
//    rsp = rsp - 8
//    memory[ss:rsp] = rax
//

/*
0000000000000000 <f1>:
   0:   55                      push   %rbp
   1:   48 89 e5                mov    %rsp,%rbp
   4:   b8 00 00 00 00          mov    $0x0,%eax
   9:   5d                      pop    %rbp
   a:   c3                      ret
*/
int f1(void) {
  return 0;
}

// The System V ABI specifies that parameters are passed in the registers
// `rdi`, `rsi`, `rdx`, `rcx`, `r8`, and `r9`. Further arguments are passed on
// the stack in reverse order, but that won't be supported.
/*
000000000000000b <f2>:
   b:   55                      push   %rbp
   c:   48 89 e5                mov    %rsp,%rbp
   f:   48 89 7d f8             mov    %rdi,-0x8(%rbp)
  13:   48 8b 45 f8             mov    -0x8(%rbp),%rax
  17:   5d                      pop    %rbp
  18:   c3                      ret
*/
int f2(int x) {
  return x;
}

/*
0000000000000019 <f3>:
  19:   55                      push   %rbp
  1a:   48 89 e5                mov    %rsp,%rbp
  1d:   48 89 7d f8             mov    %rdi,-0x8(%rbp)
  21:   48 89 75 f0             mov    %rsi,-0x10(%rbp)
  25:   48 89 55 e8             mov    %rdx,-0x18(%rbp)
  29:   b8 00 00 00 00          mov    $0x0,%eax
  2e:   5d                      pop    %rbp
  2f:   c3                      ret
*/
int f3(int x, int y, int z) {
  return 0;
}

/*
0000000000000030 <f4>:
  30:   55                      push   %rbp
  31:   48 89 e5                mov    %rsp,%rbp
  34:   48 c7 45 f8 00 00 00    movq   $0x0,-0x8(%rbp)
  3b:   00
  3c:   48 8b 45 f8             mov    -0x8(%rbp),%rax
  40:   5d                      pop    %rbp
  41:   c3                      ret
*/
int f4(void) {
  int x = 0;
  return x;
}

/*
0000000000000042 <f5>:
  42:   55                      push   %rbp
  43:   48 89 e5                mov    %rsp,%rbp
  46:   48 c7 45 e8 00 00 00    movq   $0x0,-0x18(%rbp)
  4d:   00
  4e:   48 c7 45 f0 00 00 00    movq   $0x0,-0x10(%rbp)
  55:   00
  56:   48 c7 45 f8 00 00 00    movq   $0x0,-0x8(%rbp)
  5d:   00
  5e:   b8 00 00 00 00          mov    $0x0,%eax
  63:   5d                      pop    %rbp
  64:   c3                      ret
*/
int f5(void) {
  int x = 0;
  int y = 0;
  int z = 0;
  return 0;
}

// `leave` is exactly equivalent to:
//
//    mov %ebp, %esp
//    pop %ebp
//
// Conversely, `enter $n` is exactly equivalent to:
//
//    push %ebp
//    mov %esp, %ebp
//    sub $n, %esp
//
/*
0000000000000065 <f6>:
  65:   55                      push   %rbp
  66:   48 89 e5                mov    %rsp,%rbp
  69:   48 83 ec 10             sub    $0x10,%rsp
  6d:   48 c7 45 f8 00 00 00    movq   $0x0,-0x8(%rbp)
  74:   00
  75:   e8 00 00 00 00          call   7a <f6+0x15>
  7a:   b8 00 00 00 00          mov    $0x0,%eax
  7f:   c9                      leave
  80:   c3                      ret
*/
int f6(void) {
  int x = 0;
  f1();
  return 0;
}

/*
0000000000000081 <f7>:
  81:   55                      push   %rbp
  82:   48 89 e5                mov    %rsp,%rbp
  85:   48 83 ec 08             sub    $0x8,%rsp
  89:   48 89 7d f8             mov    %rdi,-0x8(%rbp)
  8d:   e8 00 00 00 00          call   92 <f7+0x11>
  92:   48 8b 45 f8             mov    -0x8(%rbp),%rax
  96:   c9                      leave
  97:   c3                      ret
*/
int f7(int x) {
  f1();
  return x;
}

int global = 12345;

// GCC leaves the global for the linker to figure out. Note that GCC *doesn't*
// leave accessible function calls for the linker to figure out.
/*
0000000000000098 <f8>:
  98:   55                      push   %rbp
  99:   48 89 e5                mov    %rsp,%rbp
  9c:   48 8b 05 00 00 00 00    mov    0x0(%rip),%rax        # a3 <f8+0xb>
  a3:   5d                      pop    %rbp
  a4:   c3                      ret
*/
int f8(void) {
  return global;
}

static int static_global = 67890;

// Even static globals are left to the linker.
/*
00000000000000a5 <f9>:
  a5:   55                      push   %rbp
  a6:   48 89 e5                mov    %rsp,%rbp
  a9:   48 8b 05 00 00 00 00    mov    0x0(%rip),%rax        # b0 <f9+0xb>
  b0:   5d                      pop    %rbp
  b1:   c3                      ret
*/
int f9(void) {
  return static_global;
}

int f(int x);

int main(int argc, char **argv) {
  int a = 1;
  int b = 2;
  asm ("nop\nnop");
  f(a + b);
  asm ("nop\nnop");
  a + f(b);
  asm ("nop\nnop");
  putchar(a);
  asm ("nop\nnop");
  return 0;
}
