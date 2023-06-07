// gcc -std=c99 -pedantic -w -O0 tmp.c
#include <stdio.h>

int main(void) {
  int x = 1;
  int* p = (int*)32;

  printf("sizeof (int) = %zu\n", sizeof (int));

  printf("x + x = %d\n", x + x);
  printf("p + x = %d\n", p + x); // (int)p + x * sizeof (int)
  printf("x + p = %d\n", x + p); // x * sizeof (int) + (int)p
  //printf("p + p = %d\n", p + p); // invalid

  printf("x - x = %d\n", x - x);
  printf("p - x = %d\n", p - x); // (int)p - x * sizeof (int)
  //printf("x - p = %d\n", x - p); // invalid
  printf("p - p = %d\n", p - p); // ((int)p - (int)p) / sizeof (int)

  return 0;
}
