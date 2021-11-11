#include "libcalc.h"
#include <stdio.h>
#include <stdlib.h>

void die(char *msg) {
  printf("%s\n", msg);
  exit(1);
}

int main(int argc, char **argv) {
  calc_init();

  char source[256];
  printf("> ");
  fgets(source, sizeof(source), stdin);

  expr_type expr;
  if (calc_parse(source, &expr) != ERR_SUCCESS)
    die("unable to parse expression");

  char *result;

  expr_type simplified_expr;
  calc_simplify(expr, &simplified_expr);
  
  if (calc_expression_to_string(simplified_expr, &result) != ERR_SUCCESS)
    die("unable to print expression to string");

  printf("\n%s\n", result);

  release_handle(expr);
  release_handle(simplified_expr);
  return 0;
}
