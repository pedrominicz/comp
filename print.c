#include "all.h"

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

void print_expr(struct expr* expr) {
  switch (expr->kind) {
    case EXPR_VAR: fputs(expr->var->name, stderr); return;
    case EXPR_NUM: fprintf(stderr, "%d", expr->value); return;
    // binary expressions
    case EXPR_ADD: case EXPR_SUB:
    case EXPR_MUL: case EXPR_DIV:
    case EXPR_EQ: case EXPR_LE: {
      static char* op[] = {
        [EXPR_ADD]  = "+",
        [EXPR_SUB]  = "-",
        [EXPR_MUL]  = "*",
        [EXPR_DIV]  = "/",
        [EXPR_EQ]   = "==",
        [EXPR_LE]   = "<=",
      };
      fputc('(', stderr);
      print_expr(expr->op.lhs);
      fprintf(stderr, " %s ", op[expr->kind]);
      print_expr(expr->op.rhs);
      fputc(')', stderr);
      return;
    }
    // unary expressions
    case EXPR_NEG: case EXPR_NOT: case EXPR_REF: case EXPR_DEREF: {
      static char* op[] = {
        [EXPR_NEG]    = "-",
        [EXPR_NOT]    = "!",
        [EXPR_REF]    = "&",
        [EXPR_DEREF]  = "*",
      };
      fprintf(stderr, "(%s", op[expr->kind]);
      print_expr(expr->op.lhs);
      fputc(')', stderr);
      return;
    }
    case EXPR_CALL:
      fprintf(stderr, "%s(", expr->call.fn);
      if (expr->call.args[0]) {
        print_expr(expr->call.args[0]);
        for (int i = 1; i < MAX_ARGS && expr->call.args[i]; ++i) {
          fputs(", ", stderr);
          print_expr(expr->call.args[i]);
        }
      }
      fputc(')', stderr);
      return;
  }

  impossible();
}

void print_stmt_indented(struct stmt* stmt, bool should_indent, int indent) {
  if (should_indent) fprintf(stderr, "%*s", indent, "");

  switch (stmt->kind) {
    case STMT_LET:
      fprintf(stderr, "let %s = ", stmt->let.var->name);
      print_expr(stmt->let.value);
      fputs(";\n", stderr);
      return;
    case STMT_ASSIGN:
      print_expr(stmt->assign.place);
      fputs(" = ", stderr);
      print_expr(stmt->assign.value);
      fputs(";\n", stderr);
      return;
    case STMT_EXPR:
      print_expr(stmt->expr);
      fputs(";\n", stderr);
      return;
    case STMT_RETURN:
      if (stmt->return_) {
        fputs("return ", stderr);
        print_expr(stmt->return_);
      } else {
        fputs("return", stderr);
      }
      fputs(";\n", stderr);
      return;
    case STMT_BLOCK:
      if (stmt->block) {
        fputs("{\n", stderr);
        for (stmt = stmt->block; stmt; stmt = stmt->next) {
          print_stmt_indented(stmt, true, indent + 2);
        }
        fprintf(stderr, "%*s}\n", indent, "");
      } else {
        fputs("{}\n", stderr);
      }
      return;
    case STMT_IF:
      fputs("if (", stderr);
      print_expr(stmt->if_.cond);
      fputs(") ", stderr);
      print_stmt_indented(stmt->if_.then, false, indent);
      if (stmt->if_.else_) {
        fprintf(stderr, "%*selse ", indent, "");
        print_stmt_indented(stmt->if_.else_, false, indent);
      }
      return;
    case STMT_WHILE:
      fputs("while (", stderr);
      print_expr(stmt->while_.cond);
      fputs(") ", stderr);
      print_stmt_indented(stmt->while_.loop, false, indent);
      return;
  }

  impossible();
}

void print_stmt(struct stmt* stmt) {
  print_stmt_indented(stmt, false, 0);
}

void print_fn(struct fn* fn) {
  if (!fn) return;

  fprintf(stderr, "fn %s(", fn->name);
  if (fn->args_count) {
    fputs(fn->locals[0]->name, stderr);
    for (int i = 1; i < fn->args_count; ++i) {
      fprintf(stderr, ", %s", fn->locals[i]->name);
    }
  }
  fputs(") ", stderr);
  print_stmt(fn->body);

  print_fn(fn->next);
}