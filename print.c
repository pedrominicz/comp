#include "all.h"

void print_expr(struct expr* expr) {
  if (!expr) return;

  switch (expr->kind) {
    case EXPR_VAR: fputs(expr->var, stderr); break;
    case EXPR_NUM: fprintf(stderr, "%d", expr->value); break;
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
      print_expr(expr->binary_expr.lhs);
      fprintf(stderr, " %s ", op[expr->kind]);
      print_expr(expr->binary_expr.rhs);
      fputc(')', stderr);
      break;
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
      print_expr(expr->unary_expr.lhs);
      fputc(')', stderr);
      break;
    }
    case EXPR_CALL:
      fprintf(stderr, "%s(", expr->call_expr.fn);
      if (expr->call_expr.args[0]) {
        print_expr(expr->call_expr.args[0]);
        for (int i = 1; i < 6 && expr->call_expr.args[i]; ++i) {
          fputs(", ", stderr);
          print_expr(expr->call_expr.args[i]);
        }
      }
      fputc(')', stderr);
      break;
    default: die(0, "%s:%d: impossible", __FILE__, __LINE__);
  }
}

void print_stmt_indented(struct stmt* stmt, bool should_indent, int indent) {
  if (!stmt) return;

  if (should_indent) fprintf(stderr, "%*s", indent, "");

  switch (stmt->kind) {
    case STMT_LET:
      fprintf(stderr, "let %s = ", stmt->let_stmt.var);
      print_expr(stmt->let_stmt.value);
      fputs(";\n", stderr);
      break;
    case STMT_ASSIGN:
      fprintf(stderr, "%s = ", stmt->assign_stmt.var);
      print_expr(stmt->assign_stmt.value);
      fputs(";\n", stderr);
      break;
    case STMT_EXPR:
      print_expr(stmt->expr);
      fputs(";\n", stderr);
      break;
    case STMT_RETURN:
      if (stmt->return_stmt.value) {
        fputs("return ", stderr);
        print_expr(stmt->return_stmt.value);
      } else {
        fputs("return", stderr);
      }
      fputs(";\n", stderr);
      break;
    case STMT_BLOCK:
      if (stmt->block_stmt.body) {
        fputs("{\n", stderr);
        print_stmt_indented(stmt->block_stmt.body, true, indent + 2);
        fprintf(stderr, "%*s}\n", indent, "");
      } else {
        fputs("{}\n", stderr);
      }
      break;
    case STMT_IF:
      fputs("if (", stderr);
      print_expr(stmt->if_stmt.cond);
      fputs(") ", stderr);
      print_stmt_indented(stmt->if_stmt.then, false, indent);
      if (stmt->if_stmt.else_) {
        fprintf(stderr, "%*selse ", indent, "");
        print_stmt_indented(stmt->if_stmt.else_, false, indent);
      }
      break;
    case STMT_WHILE:
      fputs("while (", stderr);
      print_expr(stmt->while_stmt.cond);
      fputs(") ", stderr);
      print_stmt_indented(stmt->while_stmt.loop, false, indent);
      break;
    default: die(0, "%s:%d: impossible", __FILE__, __LINE__);
  }

  print_stmt_indented(stmt->next, should_indent, indent);
}

void print_stmt(struct stmt* stmt) {
  print_stmt_indented(stmt, false, 0);
}

void print_fn(struct fn* fn) {
  if (!fn) return;

  fprintf(stderr, "fn %s(", fn->name);
  if (fn->args[0]) {
    fputs(fn->args[0], stderr);
    for (int i = 1; i < 6 && fn->args[i]; ++i) {
      fprintf(stderr, ", %s", fn->args[i]);
    }
  }
  fputs(") ", stderr);
  print_stmt(fn->body);

  print_fn(fn->next);
}
