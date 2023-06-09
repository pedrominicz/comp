#include "all.h"

void die(int line, char* msg, ...) {
  if (line) fprintf(stderr, "line %d: ", line);

  va_list ap;

  va_start(ap, msg);
  vfprintf(stderr, msg, ap);
  va_end(ap);

  fputc('\n', stderr);

  exit(1);
}

void* alloc(int size) {
  void* buffer = calloc(1, size);
  if (!buffer) die(0, "failed to allocate memory");
  return buffer;
}

int main(int argc, char** argv) {
  if (argc != 2) die(0, "invalid number of arguments");

  parse_init(argv[1]);
  //gen(parse());
  print_fn(parse());

  return 0;
}
