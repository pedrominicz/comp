#include <stdio.h>

#define int long long

int message(int number) {
  if(!number) {
    puts("Hello, world!");
  } else {
    printf("%d\n", number);
  }
  return 0;
}
