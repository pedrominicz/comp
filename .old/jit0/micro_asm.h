#include <stdarg.h>

#define xmm(n) (n)

typedef struct {
  char *dest;
} micro_asm;

void asm_write(micro_asm *a, int n, ...) {
  va_list bytes;
  va_start(bytes, n);
  for (int i = 0; i < n; ++i) *(a->dest++) = (char)va_arg(bytes, int);
  va_end(bytes);
}

void movsd_reg_memory(micro_asm *a, char reg, char disp)
{ asm_write(a, 5, 0xf2, 0x0f, 0x11, 0x47 | reg << 3, disp); }

void movsd_memory_reg(micro_asm *a, char disp, char reg)
{ asm_write(a, 5, 0xf2, 0x0f, 0x10, 0x47 | reg << 3, disp); }

void movsd_reg_reg(micro_asm *a, char src, char dst)
{ asm_write(a, 4, 0xf2, 0x0f, 0x11, 0xc0 | src << 3 | dst); }

void mulsd(micro_asm *a, char src, char dst)
{ asm_write(a, 4, 0xf2, 0x0f, 0x59, 0xc0 | dst << 3 | src); }

void addsd(micro_asm *a, char src, char dst)
{ asm_write(a, 4, 0xf2, 0x0f, 0x58, 0xc0 | dst << 3 | src); }

void subsd(micro_asm *a, char src, char dst)
{ asm_write(a, 4, 0xf2, 0x0f, 0x5c, 0xc0 | dst << 3 | src); }

void movpd_reg_memory(micro_asm *a, char reg, char disp)
{ asm_write(a, 5, 0x66, 0x0f, 0x11, 0x47 | reg << 3, disp); }

void movpd_memory_reg(micro_asm *a, char disp, char reg)
{ asm_write(a, 5, 0x66, 0x0f, 0x10, 0x47 | reg << 3, disp); }

void addpd_memory_reg(micro_asm *a, char disp, char reg)
{ asm_write(a, 5, 0x66, 0x0f, 0x58, 0x47 | reg << 3, disp); }
