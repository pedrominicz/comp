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

void print_token(struct token tk) {
  static char* name[] = {
    [TK_EOF]        = "EOF",
    [TK_IDENT]      = "IDENT",
    [TK_NUM]        = "NUM",
    [TK_LPAREN]     = "LPAREN",
    [TK_RPAREN]     = "RPAREN",
    [TK_ADD]        = "ADD",
    [TK_SUB]        = "SUB",
    [TK_MUL]        = "MUL",
    [TK_DIV]        = "DIV",
    [TK_EQ]         = "EQ",
    [TK_NE]         = "NE",
    [TK_LT]         = "LT",
    [TK_LE]         = "LE",
    [TK_GT]         = "GT",
    [TK_GE]         = "GE",
    [TK_NOT]        = "NOT",
    [TK_REF]        = "REF",
    [TK_ASSIGN]     = "ASSIGN",
    [TK_SEMICOLON]  = "SEMICOLON",
    [TK_COMMA]      = "COMMA",
    [TK_LBRACKET]   = "LBRACKET",
    [TK_RBRACKET]   = "RBRACKET",
    [TK_RETURN]     = "RETURN",
    [TK_LBRACE]     = "LBRACE",
    [TK_RBRACE]     = "RBRACE",
    [TK_IF]         = "IF",
    [TK_ELSE]       = "ELSE",
    [TK_FOR]        = "FOR",
    [TK_WHILE]      = "WHILE",
    [TK_LET]        = "LET",
    [TK_FN]         = "FN",
  };

  fprintf(stderr, "%s\t%d\t%s\n", tk.str, tk.line, name[tk.kind]);
}
