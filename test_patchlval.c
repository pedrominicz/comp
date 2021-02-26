main() {
  int a;
  a = 10;
  printf("a: %d\n", a);
  a = *(char*)&a;
  return 0;
}
