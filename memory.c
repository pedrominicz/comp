#include <stdlib.h>

#include "memory.h"

void* reallocate(void* pointer, size_t oldSize, size_t newSize) {
  (void)oldSize;

  // The behavior of `realloc(pointer, 0)` is not standardized, i.e. cannnot be
  // relied on.
  if(newSize == 0) {
    free(pointer);
    return NULL;
  }

  void* result = realloc(pointer, newSize);
  if(result == NULL) exit(1);
  return result;
}
