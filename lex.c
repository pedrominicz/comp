#include "prelude.h"

static char* current;
static char* start;
static int line;

static struct token new_token(int kind) {
  struct token tk = { kind, start, current - start, line };
  return tk;
}

static void skip_whitespace(void) {
  for (;;) {
    switch (*current) {
      case ' ': case '\r': case '\t': ++current; break;
      case '\n': ++current; ++line; break;
      default: return;
    }
  }
}

void lex_init(char* source) {
  current = source;
  start = source;
  line = 1;
}

struct token lex_next_token(void) {
  skip_whitespace();

  start = current;

  if (*current == '\0') return new_token(TK_EOF);

  if (isdigit(*current)) {
    while (isdigit(*current)) ++current;
    return new_token(TK_NUM);
  }

  switch (*current) {
    case '(': ++current; return new_token(TK_LPAREN);
    case ')': ++current; return new_token(TK_RPAREN);
    case '+': ++current; return new_token(TK_ADD);
    case '-': ++current; return new_token(TK_SUB);
    case '*': ++current; return new_token(TK_MUL);
    case '/': ++current; return new_token(TK_DIV);
  }

  die(line, "unexpected character: '%c'", *current);
  return (struct token){0}; // silence warning
}
