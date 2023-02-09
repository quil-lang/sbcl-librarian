#ifndef _libcalc_h
#define _libcalc_h

#if defined(CALC_API_BUILD)
#  if defined(_WIN64)
#    define CALC_API __declspec(dllexport)
#  elif defined(__ELF__)
#    define CALC_API __attribute__ ((visibility ("default")))
#  else
#    define CALC_API
# endif
#else
#  if defined(_WIN64)
#    define CALC_API __declspec(dllimport)
#  else
#  define CALC_API
#  endif
#endif

extern CALC_API void (*lisp_release_handle)(void* handle);
extern CALC_API int (*lisp_handle_eq)(void* a, void* b);
extern CALC_API void (*lisp_enable_debugger)();
extern CALC_API void (*lisp_disable_debugger)();
extern CALC_API void (*lisp_gc)();
/* types */
struct expr_type__ { int unused; }; typedef struct expr_type__ *expr_type;
typedef enum { ERR_SUCCESS = 0, ERR_FAIL = 1, } err_t;
/* functions */
extern CALC_API err_t (*calc_int_literal)(int value, expr_type *result);
extern CALC_API err_t (*calc_int_literal_value)(expr_type expr, int *result);
extern CALC_API err_t (*calc_int_literal_p)(expr_type obj, int *result);
extern CALC_API err_t (*calc_sum_expression)(expr_type left, expr_type right, expr_type *result);
extern CALC_API err_t (*calc_sum_expression_left_arg)(expr_type expr, expr_type *result);
extern CALC_API err_t (*calc_sum_expression_right_arg)(expr_type expr, expr_type *result);
extern CALC_API err_t (*calc_sum_expression_p)(expr_type expr, int *result);
extern CALC_API err_t (*calc_simplify)(expr_type expr, expr_type *result);
extern CALC_API err_t (*calc_parse)(char* source, expr_type *result);
extern CALC_API err_t (*calc_expression_to_string)(expr_type expr, char* *result);
extern CALC_API err_t (*calc_remove_zeros)(expr_type expr, expr_type *result);
CALC_API int init(char* core);

#endif
