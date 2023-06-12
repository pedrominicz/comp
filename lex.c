#include "all.h"

static char* current;
static char* start;
static int line;

static struct token new_token(int kind) {
  return (struct token){ kind, line, strndup(start, current - start) };
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

struct token lex(void) {
  skip_whitespace();

  start = current;

  if (*current == '\0') return new_token(TK_EOF);

  if (isdigit(*current)) {
    while (isdigit(*current)) ++current;
    return new_token(TK_NUM);
  }

  if (isalpha(*current)) {
    while (isalpha(*current) || isdigit(*current)) ++current;
    struct token tk = new_token(TK_IDENT);

    if (!strcmp("return", tk.str)) tk.kind = TK_RETURN;
    if (!strcmp("if",     tk.str)) tk.kind = TK_IF;
    if (!strcmp("else",   tk.str)) tk.kind = TK_ELSE;
    if (!strcmp("for",    tk.str)) tk.kind = TK_FOR;
    if (!strcmp("while",  tk.str)) tk.kind = TK_WHILE;
    if (!strcmp("let",    tk.str)) tk.kind = TK_LET;
    if (!strcmp("fn",     tk.str)) tk.kind = TK_FN;
    if (!strcmp("true",   tk.str)) tk.kind = TK_TRUE;
    if (!strcmp("false",  tk.str)) tk.kind = TK_FALSE;

    return tk;
  }

  switch (*current++) {
    case '(': return new_token(TK_LPAREN);
    case ')': return new_token(TK_RPAREN);
    case '+': return new_token(TK_ADD);
    case '-': return new_token(TK_SUB);
    case '*': return new_token(TK_MUL);
    case '/': return new_token(TK_DIV);
    case '&': return new_token(TK_REF);
    case '=': return new_token(*current == '=' ? ++current, TK_EQ : TK_ASSIGN);
    case '!': return new_token(*current == '=' ? ++current, TK_NE : TK_NOT);
    case '<': return new_token(*current == '=' ? ++current, TK_LE : TK_LT);
    case '>': return new_token(*current == '=' ? ++current, TK_GE : TK_GT);
    case ';': return new_token(TK_SEMICOLON);
    case ',': return new_token(TK_COMMA);
    case '[': return new_token(TK_LBRACKET);
    case ']': return new_token(TK_RBRACKET);
    case '{': return new_token(TK_LBRACE);
    case '}': return new_token(TK_RBRACE);
  }

  die(line, "unexpected character: '%c'", *--current);
  return (struct token){0}; // silence warning
}
