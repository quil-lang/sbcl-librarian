#define CALC_API_BUILD

#include "libcalc.h"

void (*lisp_release_handle)(void* handle);
int (*lisp_handle_eq)(void* a, void* b);
void (*lisp_enable_debugger)();
void (*lisp_disable_debugger)();
void (*lisp_gc)();
err_t (*calc_int_literal)(int value, expr_type *result);
err_t (*calc_int_literal_value)(expr_type expr, int *result);
err_t (*calc_int_literal_p)(expr_type obj, int *result);
err_t (*calc_sum_expression)(expr_type left, expr_type right, expr_type *result);
err_t (*calc_sum_expression_left_arg)(expr_type expr, expr_type *result);
err_t (*calc_sum_expression_right_arg)(expr_type expr, expr_type *result);
err_t (*calc_sum_expression_p)(expr_type expr, int *result);
err_t (*calc_simplify)(expr_type expr, expr_type *result);
err_t (*calc_parse)(char* source, expr_type *result);
err_t (*calc_expression_to_string)(expr_type expr, char* *result);
err_t (*calc_remove_zeros)(expr_type expr, expr_type *result);

extern int initialize_lisp(int argc, char **argv);

CALC_API int init(char* core) {
  static int initialized = 0;
  char *init_args[] = {"", "--core", core, "--noinform", };
  if (initialized) return 1;
  if (initialize_lisp(4, init_args) != 0) return -1;
  initialized = 1;
  return 0; }