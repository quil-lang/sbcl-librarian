/* Use a layer of indirection to emulate what Lisp FFI does. */

void _c_empty_function(void) {
  return;
}

int _c_opaque_function(void **out_ptr) {
  *out_ptr = (void*)12345;
  return 0;
}

void (*c_empty_function)(void) = _c_empty_function;
int (*c_opaque_function)(void**) = _c_opaque_function;
