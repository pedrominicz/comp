#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "vm.h"

static void repl(void) {
  char line[1024];
  for(;;) {
    printf("> ");

    if(!fgets(line, sizeof(line), stdin)) {
      printf("\n");
      break;
    }

    interpret(line);
  }
}

static char* readFile(const char* path) {
  FILE* file = fopen(path, "rb");
  if(file == NULL) {
    fprintf(stderr, "Could not open file \"%s\".\n", path);
    exit(1);
  }

  fseek(file, 0L, SEEK_END);
  size_t fileSize = ftell(file);
  rewind(file);

  char* buffer = malloc(fileSize + 1);
  if(buffer == NULL) {
    fprintf(stderr, "Not enough memory to read \"%s\".\n", path);
    exit(1);
  }

  size_t bytesRead = fread(buffer, sizeof(char), fileSize, file);
  if(bytesRead < fileSize) {
    fprintf(stderr, "Could not read file \"%s\".\n", path);
    exit(1);
  }

  buffer[bytesRead] = '\0';

  fclose(file);
  return buffer;
}

static void runFile(const char* path) {
  char* source = readFile(path);
  InterpretResult result = interpret(source);
  free(source); 

  if(result != INTERPRET_OK) exit(1);
}

int main(int argc, const char** argv) {
  initVM();

  if(argc == 1) {
    repl();
  } else if(argc == 2) {
    runFile(argv[1]);
  } else {
    fprintf(stderr, "Usage: %s [path]\n", argv[0]);
    exit(1);
  }

  freeVM();
  return 0;
}
